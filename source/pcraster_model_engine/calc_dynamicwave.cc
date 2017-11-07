// #define DEBUG_DEVELOP 1

#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_DYNAMICWAVE
#include "calc_dynamicwave.h"
#define INCLUDED_CALC_DYNAMICWAVE
#endif

// Library headers.
#ifndef INCLUDED_BOOST_FORMAT
#include <boost/format.hpp>
#define INCLUDED_BOOST_FORMAT
#endif
// PCRaster library headers.
#ifndef INCLUDED_COM_CSFCELL
#include "com_csfcell.h"
#define INCLUDED_COM_CSFCELL
#endif
#ifndef INCLUDED_COM_MVOP
#include "com_mvop.h"
#define INCLUDED_COM_MVOP
#endif
#ifndef INCLUDED_APPARGS
#include "appargs.h"      // appDynamicWaveRoughness
#define INCLUDED_APPARGS
#endif

// Module headers.
#ifndef INCLUDED_CALC_VFIELD
#include "calc_vfield.h"
#define INCLUDED_CALC_VFIELD
#endif
#ifndef INCLUDED_CALC_SCOPEDLDDGRAPH
#include "calc_scopedlddgraph.h"
#define INCLUDED_CALC_SCOPEDLDDGRAPH
#endif
#ifndef INCLUDED_CALC_LOOKUPTABLE
#include "calc_lookuptable.h"
#define INCLUDED_CALC_LOOKUPTABLE
#endif
#ifndef INCLUDED_CALC_EXECARGUMENTS
#include "calc_execarguments.h"
#define INCLUDED_CALC_EXECARGUMENTS
#endif
#ifndef INCLUDED_CALC_FIELD
#include "calc_field.h"
#define INCLUDED_CALC_FIELD
#endif
#ifndef INCLUDED_CALC_NONSPATIAL
#include "calc_nonspatial.h"
#define INCLUDED_CALC_NONSPATIAL
#endif
#ifndef INCLUDED_CALC_DOMAINERROR
#include "calc_domainerror.h"
#define INCLUDED_CALC_DOMAINERROR
#endif
#ifndef INCLUDED_CALC_DOWNSTREAMVISITOR
#include "calc_downstreamvisitor.h"
#define INCLUDED_CALC_DOWNSTREAMVISITOR
#endif
#ifndef INCLUDED_CALC_TIMESLICEVISITOR
#include "calc_TimeSliceVisitor.h"
#define INCLUDED_CALC_TIMESLICEVISITOR
#endif

/*!
  \file
  This file contains the implementation of the DynamicWave class.
*/



//------------------------------------------------------------------------------


namespace calc {

static   std::string prefix;

DynamicWave     dynamicWave;
KinematicWave   kinematicWave; // kinwavestate, kinwaveflux
Kinematic       builtIn_kinematic;
// Muskingum       builtIn_muskingumtimestep;
Muskingum       builtIn_muskingum;
LookupState     builtIn_lookupstate;
LookupPotential builtIn_lookuppotential;

//! call setPrefixSort on ctor
class DynamicWaveExecArguments : public ExecArguments {
  public:
    DynamicWaveExecArguments(
      const Operator& op,
      RunTimeEnv *rte,
      size_t nrActualArgs):
       ExecArguments(op,rte,nrActualArgs)
    {
      const LookupTable *tab=
        dynamic_cast<const LookupTable *>(firstNonFieldInput());
      PRECOND(tab);
      ((LookupTable *)tab)->setPrefixStableSort(1);
    }
};


//! Accumulation through a directed graph, like LddGraph
class DWVisitor : public TimeSliceVisitor, public DynamicWaveTable
{
private:
   DWVisitor &          operator=           (DWVisitor  const& rhs);
                   DWVisitor                (DWVisitor  const& rhs);

   CurrentSliceInfo       d_csi;

   //! _state
   double*                d_state;

   //! _flux
   double*                d_flux;

   const  LookupTable&    d_tab;

   const VField<INT4 >&   d_profileId;
   const float*           d_inflow;
   const VField<REAL4>&   d_bottomLevel;
   const VField<REAL4>&   d_roughness;
   const VField<REAL4>&   d_segmentLength;
   const VField<UINT1>&   d_constantState;

   //! set to 0 at start visit per Pit
   std::vector<double>    d_fluxTS;

   typedef double (* QDyn)(double Roughness, double Aact, double Sf, double R);
   const QDyn             d_qDyn;
   static  double qDynChezy(double,double,double,double);
   static  double qDynManning(double,double,double,double);


   void initVertexBeforeSlice(size_t v);
   void visitEdge        (size_t up, size_t down);
   void finishVertex     (size_t v);

   double dwPotential    (size_t v) const;
   double dwFlux         (size_t up,size_t down) const;
   double dwLookup       (size_t v,double  fromVal,
                          LookupColumns fromCol,
                          LookupColumns toCol) const;

public:
                    DWVisitor           (double* state, double* flux,
                                           const  LookupTable& tab,
                                           const  VField<INT4>&  profileId,
                                           const  LddGraph& lg,
                                           const  float*          inflow,
                                           const  VField<float>&  bottomLevel,
                                           const  VField<float>&  roughness,
                                           const  VField<float>&  segmentLength,
                                           const  VField<INT4>&   nrTimeSlices,
                                           const  Field&          timestepInSecs,
                                           const  VField<UINT1>&  constantState);

  /* virtual */    ~DWVisitor               ();

  void             initPerCatchmentSlice    (CurrentSliceInfo const& csi);

};

DWVisitor::DWVisitor(double* state, double* flux,
                 const  LookupTable& tab,
                 const  VField<INT4>&  profileId,
                 const  LddGraph& lg,
                 const  float*          inflow,
                 const  VField<float>&  bottomLevel,
                 const  VField<float>&  roughness,
                 const  VField<float>&  segmentLength,
                 const  VField<INT4>&   nrTimeSlices,
                 const  Field&          timestepInSecs,
                 const  VField<UINT1>&  constantState):
  TimeSliceVisitor(lg,nrTimeSlices,timestepInSecs),
  d_state(state),
  d_flux(flux),

  d_tab(tab),
  d_profileId(profileId),
  d_inflow(inflow),
  d_bottomLevel(bottomLevel),
  d_roughness(roughness),
  d_segmentLength(segmentLength),
  d_constantState(constantState),

  d_fluxTS(lg.nrVertices()),
  d_qDyn((appDynamicWaveRoughness==APP_DWR_CHEZY)?qDynChezy : qDynManning)
{
}

DWVisitor::~DWVisitor()
{
}

void DWVisitor::initPerCatchmentSlice(CurrentSliceInfo const& csi)
{
  d_csi = csi;
}

void DWVisitor::initVertexBeforeSlice(size_t v)
{
  d_fluxTS[v]=0;
}


//! identical structure as LddRouting::visitEdge
/*!
 * called for each edge from up to down in downstreamorder
 * calculate transport (flux) from up to down
 *
 * \pre
 *   d_fluxTs[up] is at entrance the sum of up's
 *   upstream fluxes, 0 if up has no upstream cells
 *   finishVertex does that
 */
void DWVisitor::visitEdge(size_t up, size_t down)
{
  // _potential = g(...) FTTB integrated in dwFlux()

  if (d_constantState[up]!=1)
    d_state[up] += d_inflow[up]/d_csi.nrTimeSlices;

  d_fluxTS[up] = std::min<double>(
                   d_state[up],
                   std::max(0.0,dwFlux(up,down))*d_csi.sliceInSecs);
  DEVELOP_PRECOND(d_fluxTS[up] >= 0);

  if (d_constantState[up]!=1)
    d_state[up]-=d_fluxTS[up];
  DEVELOP_PRECOND(d_state[up] >= 0);
  d_flux[up]+=d_fluxTS[up];

  // send calculated flux to down
  d_fluxTS[down]+=d_fluxTS[up];
}

void DWVisitor::finishVertex(size_t v)
{
  // d_fluxTS is now the sum of upstream fluxes
  if (d_constantState[v]!=1)
    d_state[v]+=d_fluxTS[v];
}

double tableLookup(
    const  LookupTable& tab,
    INT4   profileVal,
    float  fromVal,
    DynamicWaveTable::LookupColumns fromCol,
    DynamicWaveTable::LookupColumns toCol)
{
  double result;
  std::vector<float> prefixKey(1,profileVal);
  bool r=tab.interpolate(result, prefixKey,fromVal,fromCol,toCol);
  if (!r) {
     // enum   LookupColumns { profileId=0,H=1,A=2,P=3 };
     PRECOND(DynamicWaveTable::profileId==0);
     PRECOND(DynamicWaveTable::H==1);
     PRECOND(DynamicWaveTable::A==2);
     PRECOND(DynamicWaveTable::P==3);
     const char *names[] = {
       "ProfileId","H","A","P" };

     std::string msg(
      ( boost::format("No match for Key{ProfileId=%1%,%2%=%3%} -> %4%") %
      profileVal % names[fromCol] % fromVal % names[toCol]).str());
     throw DomainError(msg);
  }
  return result;
}

double DWVisitor::dwLookup(
    size_t v,
    double  fromVal,
    LookupColumns fromCol,
    LookupColumns toCol) const
{
  return tableLookup(d_tab,d_profileId[v],(float)fromVal,fromCol,toCol);
}


double DWVisitor::dwPotential(size_t v) const
{
  PRECOND(d_state[v] >= 0);
  double a = d_state[v]/d_segmentLength[v]; // m2
  double h = dwLookup(v,a,A,H);  // m
  return h+d_bottomLevel[v];
}


inline static double sqrtSf(
    double sF)
{
 // FTTB holds if no negative fluxes, sF always >= 0
 DEVELOP_PRECOND(sF >=0);
  if (sF >= 0)
        return std::sqrt(sF);
  return -std::sqrt(-sF);
}

//! calculates Q in m3/sec
double DWVisitor::qDynChezy(
    double Roughness,
    double Aact,
    double Sf,
    double R)
{
  // Roughness is Chezy-coefficient
  return Roughness  * Aact * sqrtSf(Sf)  * std::sqrt(R);
}

//! calculates Q in m3/sec
double DWVisitor::qDynManning(
    double Roughness,
    double Aact,
    double Sf,
    double R)
{
  // Roughness is Manning coefficient
  return sqrtSf(Sf)*Aact*std::pow(R,2.0/3)/Roughness;
}

/*
 * \param v    the upstream cell
 * \param down the downstream cell
 */
double DWVisitor::dwFlux(size_t v, size_t down) const
{
 double pot=dwPotential(v);
 double dsPot=dwPotential(down);
 if (dsPot > pot) {
   // FTTB no negative fluxes, e.g. no water uphill
   return 0.0;
 }
 double h=pot-d_bottomLevel[v]; // water depth in cell
 PRECOND(h>=0);
 double a=dwLookup(v,h,H,A); //  Aact natte oppervlak
 PRECOND(a >= 0);
 double p=dwLookup(v,h,H,P); // wetted perimeter voor de gegeven waterdiepte
 PRECOND(p >= 0);
 double aDivP = (p > 0 ? a/p : 0);
 double sF=(pot-dsPot)/d_segmentLength[v];
 // FTTB holds if no negative fluxes, sF always >= 0
 PRECOND(sF >=0);
 // qDyn is qDynChezy or qDynManning
 double  fluxPerSecond = d_qDyn(d_roughness[v], a, sF, aDivP);
 return fluxPerSecond;
}
} // namespace calc




//------------------------------------------------------------------------------
// DEFINITION OF STATIC DYNAMICWAVE MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF DYNAMICWAVE MEMBERS
//------------------------------------------------------------------------------

calc::DynamicWave::DynamicWave()
{
}



/* NOT IMPLEMENTED
//! Copy constructor.
calc::DynamicWave::DynamicWave(DynamicWave const& rhs)

  : Base(rhs)

{
}
*/



calc::DynamicWave::~DynamicWave()
{
}



/* NOT IMPLEMENTED
//! Assignment operator.
calc::DynamicWave& calc::DynamicWave::operator=(DynamicWave const& rhs)
{
  if (this != &rhs) {
  }
  return *this;
}
*/

void calc::DynamicWave::exec(RunTimeEnv* rte,
                             const Operator& op,
                             size_t nrActualInputs) const
{
  DynamicWaveExecArguments a(op,rte,nrActualInputs);
  const LookupTable *tab=
    dynamic_cast<const LookupTable *>(a.firstNonFieldInput());

  enum Args {
   argProfileId=0,
   argLdd=1,
   argOldState=2,
   argInflow=3,
   argBottomLevel=4,
   argRoughness=5,
   argSegmentLength=6,
   argNrTimeSlices=7,
   argTimestepInSecs=8,
   argConstantState=9
  };

  ScopedLddGraph      lg(rte,a[argLdd]);

  // field length, ldd known to be spatial
  size_t fLen=a[argLdd].nrValues();

  BitField mvField(fLen);
  POSTCOND(mvField.none());

  const VField<INT4>      profileId(a[argProfileId],mvField);
  const VField<REAL4>      oldState(a[argOldState],mvField);
  const VField<REAL4>        inflow(a[argInflow],mvField);
  const VField<REAL4>   bottomLevel(a[argBottomLevel],mvField);
  const VField<REAL4>     roughness(a[argRoughness],mvField);
  const VField<REAL4> segmentLength(a[argSegmentLength],mvField);
  const VField<INT4>   nrTimeSlices(a[argNrTimeSlices],mvField);
  const VField<UINT1> constantState(a[argConstantState],mvField);

  PRECOND(!a[argTimestepInSecs].isSpatial());
  float timestepInSecs= a[argTimestepInSecs].src_f()[0];
  if (timestepInSecs <= 0)
    throw DomainError("timestepInSecs must be > 0");


  // check for nonspatial case
  PRECOND(!a[argNrTimeSlices].isSpatial());
  if (a[argNrTimeSlices].src_4()[0] <= 0)
      throw DomainError("nrTimeSlices must be > 0");

  // initialize newStateV with oldState
  std::vector<double> newStateV(fLen);
  lg.current().copyField<double,float>(&(newStateV[0]),oldState);
  // initialize fluxV to 0
  std::vector<double> fluxV(fLen,0.0);

  lg.setMVInput(mvField);

  // m3/channellength/sec  -> m3/Timestep
  std::vector<float> inflowM3perTimestep(fLen);
  lg.current().copyField<float,float>(&(inflowM3perTimestep[0]),inflow);
  for(size_t i=0; i < fLen; ++i)
    inflowM3perTimestep[i] *= timestepInSecs * segmentLength[i];

  DWVisitor  dwv(&(newStateV[0]),&(fluxV[0]),*tab,
                 profileId,
                 lg.current(),
                 &(inflowM3perTimestep[0]),
                 bottomLevel,
                 roughness,
                 segmentLength,
                 nrTimeSlices,
                 a[argTimestepInSecs],
                 constantState);

  dwv.visitPerCachmentSlice();

  a.createResults();
  Field& newState(a.result(0));
  Field&     flux(a.result(1));

  lg.current().copyField<float,double>(newState.dest_f(),&(newStateV[0]));

  // m3/timestep -> m3/sec
  for(size_t i=0; i < fLen; ++i)
    fluxV[i] /= timestepInSecs;
  lg.current().copyField<float,double>(flux.dest_f(),&(fluxV[0]));

  a.pushResults();
}

void calc::LookupState::exec(RunTimeEnv* rte,
                             const Operator& op,
                             size_t nrActualInputs) const
{
  DynamicWaveExecArguments a(op,rte,nrActualInputs);
  const LookupTable *tab=
    dynamic_cast<const LookupTable *>(a.firstNonFieldInput());

  Field& result(a.createResult());

  // 1 if all inputs are nonspatial
  size_t fLen=result.nrValues();

  const VField<INT4>      profileId(a[0],fLen);
  const VField<REAL4>   bottomLevel(a[1],fLen);
  const VField<REAL4> segmentLength(a[2],fLen);
  const VField<REAL4>     potential(a[3],fLen);

  PRECOND(result.cri()==CRI_f);
  float *r=result.dest_f();
  for(size_t i=0;i<fLen;++i) {
    if (com::oneIsMV(profileId[i],bottomLevel[i])|
        com::oneIsMV(segmentLength[i],potential[i]))
      pcr::setMV(r[i]);
    else {
      float h=potential[i]-bottomLevel[i];
      double a=tableLookup(*tab,profileId[i],h,
          DynamicWaveTable::H, DynamicWaveTable::A);
      r[i]=(float)(a*segmentLength[i]);
    }
  }

  a.pushResults();
}

void calc::LookupPotential::exec(RunTimeEnv* rte,
                             const Operator& op,
                             size_t nrActualInputs) const
{
  DynamicWaveExecArguments a(op,rte,nrActualInputs);
  const LookupTable *tab=
    dynamic_cast<const LookupTable *>(a.firstNonFieldInput());

  Field& result(a.createResult());

  // 1 if all inputs are nonspatial
  size_t fLen=result.nrValues();

  const VField<INT4>      profileId(a[0],fLen);
  const VField<REAL4>   bottomLevel(a[1],fLen);
  const VField<REAL4> segmentLength(a[2],fLen);
  const VField<REAL4>         state(a[3],fLen);

  PRECOND(result.cri()==CRI_f);
  float *r=result.dest_f();
  for(size_t i=0;i<fLen;++i) {
    if (com::oneIsMV(profileId[i],bottomLevel[i])|
        com::oneIsMV(segmentLength[i],state[i]))
      pcr::setMV(r[i]);
    else {
      // identical to dwPotential
      float a=state[i]/segmentLength[i]; // m2
      double h=tableLookup(*tab,profileId[i],a,
          DynamicWaveTable::A, DynamicWaveTable::H); // m
      r[i]=(float)(h+bottomLevel[i]);
    }
  }

  a.pushResults();
}

static double iterateToQnew(
    double Qin,
    double Qold,
    double q,
    double alpha,
    double beta,
    double sliceInSecs,
    double deltaX);


/*!
   \sa $OLDPCRTREE/docs/pcrmanual/manual_updates/dynWaveRouting.doc

The two kinematic variants (kinematic() and kinematicflux/kinematicstate()
have been reworked considerable in two stages. Goal was to have them and dynamicwave working in constitently together, all three use comparable units for their inputs and outputs. The two work stages were:
  - As delivered to JRC May 2006 on base of WIllem March 22 2006 mail.
  - September 2007 (see mail Willem/JRC/Cees)

The result is the October 3 2007 version and on, that is the following in pseudo code:

\code
function kinematic( ldd, Qold, q, alpha, beta, numberOfTimeSlices, dT, dX)
begin
  1 Zoek de pits en lees de waarde van numberOfTimeSlices voor de pit
  foreach pit do
(*    findcatchment *)
    2 Voor de catchment boven de pit do
    begin
      sliceInSecs :=dT/numberOfTimeSlices
      for i:=1 to numberOfTimeSlices do
        begin
          Qnew=iterateToQnew(?,Qold,q,...,sliceInSecs, dX)
          Qold:=QNew;
        end;
    end;
end; 


static double iterateToQnew(
    double Qin,
    double Qold,
    double q,
    double alpha,
    double beta,
    double sliceInSecs,
    double deltaX);
\encode

Assuming the above is what we already have, we define the following variant:

kinwavestate, kinwaveflux(ldd, previousState, qChan, alpha, beta, nrTimeslices, dT, dX)
The parameters are identical to the kinematic() ones:
 - Qold has become previousStat = previous state in m<sup>3</sup>
 - qChan inflow per m channellength m<sup>3</sup>/m/sec

So the code will compute a state and flux:

\code

kinwaveStateFluxVersion(ldd, previousState, qChan, alpha, beta, nrTimeslices, dT, dX)
begin
  1 Zoek de pits en lees de waarde van numberOfTimeSlices voor de pit
  foreach pit do
(*    findcatchment *)
    2 Voor de catchment boven de pit do
    begin
      Qold = (previousState / (alpha * dX)) ** (1/beta)   [m3/sec]
      Qsum = 0
      sliceInSecs  = dT/numberOfTimeSlices
      for i:=1 to numberOfTimeSlices do
        begin
          Qnew=iterateToQnew(?,Qold,qChan,..., sliceInSecs,dX)
          Qsum = Qsum + QNew
        end;
      State = alpha*(Qnew**beta)*dX
      Flux  = (Qsum/nrTimeslices)
    end;
end;

\endcode
*/
void calc::Kinematic::exec(
    RunTimeEnv* rte,
    const Operator& op,
    size_t nrArgs) const
{

  struct KWVisitor : public TimeSliceVisitor
  {

     float*              d_Qnew;
     CurrentSliceInfo    d_csi;

     std::vector<float>  d_QSumDownStream;
     const VField<float> d_q;
     const VField<float> d_alpha;
     const VField<float> d_beta;
     const VField<float> d_deltaX;

     void     initVertexBeforeSlice(size_t v) {
       d_QSumDownStream[v]=0.0;
     }

     bool mv(size_t pos) const {
      return
        com::oneIsMV(d_QSumDownStream[pos],d_q[pos],d_alpha[pos]) |
        com::oneIsMV(d_beta[pos],d_deltaX[pos]);
     }

     void initPerCatchmentSlice(CurrentSliceInfo const& csi)
     {
       d_csi = csi;
     }

     void visitEdge        (size_t up, size_t down) {
       // send calculated flux to down
       com::inplace_add(d_QSumDownStream[down],d_Qnew[up]);
     }

     void finishVertex     (size_t v) {
       if (mv(v)|pcr::isMV(d_Qnew[v])) {
         pcr::setMV(d_Qnew[v]);
         return;
       }
       d_Qnew[v] = (float)iterateToQnew(
           d_QSumDownStream[v], // summed Q new in for all sub-catchments
           d_Qnew[v],          /* current discharge */
           d_q[v],
           d_alpha[v],
           d_beta[v],
           d_csi.sliceInSecs,
           d_deltaX[v]);
     }

     KWVisitor (Field & qNew,
          VField<INT4>  const& nrTimeSlices,
          Field         const& timestepInSecs,
          ExecArguments const& arg,
          LddGraph      const& lg):

         TimeSliceVisitor(lg,nrTimeSlices,timestepInSecs),
         d_Qnew(qNew.dest_f()),
         d_QSumDownStream(qNew.nrValues()),

         d_q           (arg[2],qNew.nrValues()),
         d_alpha       (arg[3],qNew.nrValues()),
         d_beta        (arg[4],qNew.nrValues()),
         d_deltaX      (arg[7],qNew.nrValues())
       {
         const VField<float> Qold(arg[1],qNew.nrValues());
         // initialize QNew from Qold
         for(size_t i=0; i < qNew.nrValues(); ++i)
               d_Qnew[i] = Qold[i];
       }
  };

  ExecArguments arg(op,rte,nrArgs);
  ScopedLddGraph lgs(rte,arg[0]);
  LddGraph const& lg(lgs.current());
  Field& qNew(arg.createResult());

  const VField<INT4> nrTimeSlices(arg[5],qNew.nrValues());
  KWVisitor v(qNew,nrTimeSlices ,arg[6],arg, lg);

  v.visitPerCachmentSlice();
  arg.pushResults();
}

#include "muskingum.h"
// #include "muskingum8April2009.h"

void calc::KinematicWave::exec(
    RunTimeEnv* rte,
    const Operator& op,
    size_t nrArgs) const
{

  // KinematicWave flux/state visitor
  struct KWFSVisitor : public TimeSliceVisitor
  {

     float*              d_Qnew; // use newState buffer
     float*              d_QSum; // use flux buffer
     CurrentSliceInfo    d_csi;
     std::vector<float>  d_QSumDownStream;
     const VField<float> d_qChan;
     const VField<float> d_alpha;
     const VField<float> d_beta;
     const VField<float> d_deltaX;

     void     initVertexBeforeSlice(size_t v) {
       d_QSumDownStream[v]=0.0;
     }

     bool mv(size_t pos) const {
      return
        com::oneIsMV(d_QSumDownStream[pos],d_qChan[pos],d_alpha[pos]) |
        com::oneIsMV(d_beta[pos],d_deltaX[pos]);
     }

     void initPerCatchmentSlice(CurrentSliceInfo const& csi )
     {
       d_csi = csi;
     }

     void visitEdge        (size_t up, size_t down) {
       // send calculated flux to down
       com::inplace_add(d_QSumDownStream[down],d_Qnew[up]);
     }

     void finishVertex     (size_t v) {
       if (mv(v)|pcr::isMV(d_Qnew[v])) {
         pcr::setMV(d_Qnew[v]);
         return;
       }
       d_Qnew[v] = (float)iterateToQnew(
           d_QSumDownStream[v], // summed Q new in for all sub-catchments
           d_Qnew[v],          /* current discharge */
           d_qChan[v],
           d_alpha[v],
           d_beta[v],
           d_csi.sliceInSecs,
           d_deltaX[v]);
       d_QSum[v] += d_Qnew[v];
     }

     KWFSVisitor (
          Field & newState,
          Field & flux,
          VField<INT4>  const& nrTimeSlices,
          Field         const& timestepInSecsField,
          ExecArguments const& arg,
          LddGraph      const& lg):

         TimeSliceVisitor(lg,nrTimeSlices,timestepInSecsField),
         d_Qnew(newState.dest_f()), // use newState for Qnew buffer
         d_QSum(flux.dest_f()), // use flux for QSum buffer
         d_QSumDownStream(newState.nrValues()),
         d_qChan       (arg[2],newState.nrValues()),
         d_alpha       (arg[3],newState.nrValues()),
         d_beta        (arg[4],newState.nrValues()),
         d_deltaX      (arg[7],newState.nrValues())
       {
         const VField<float> previousState(arg[1],newState.nrValues());
         // initialize Qold as first Qnew value
         // d_Qnew is MV marker used in computeFluxAndState
         for(size_t i=0; i < newState.nrValues(); ++i) {
          if (com::oneIsMV(previousState[i],d_qChan[i],d_alpha[i]) |
             com::oneIsMV(d_beta[i],d_deltaX[i])) {
                pcr::setMV(d_Qnew[i]);
                pcr::setMV(d_QSum[i]);
          } else {
             d_Qnew[i]= std::pow(previousState[i]/(d_alpha[i]*d_deltaX[i]),1/d_beta[i]);
             d_QSum[i] = 0;
          }
        }
       }

     void computeState()
     {
       for(size_t i=0; i < d_QSumDownStream.size(); ++i) {
         if (!pcr::isMV(d_Qnew[i])) {
           // d_Qnew is marker, if that is !mv nothing is mv

           // newState = alpha*(Qnew**beta)*dX:
           d_Qnew[i]  = d_alpha[i] * std::pow(d_Qnew[i],d_beta[i]) * d_deltaX[i];
         }
       }
     }
  };

  struct DivideFluxByNrTimeSlices : public DownstreamVisitor
  {
     float*              d_flux;
     VField<INT4> const& d_nrTimeSlices;
     size_t              d_pitIdOfCurrentCatchment;

     void startCatchment(size_t pitIdOfCatchment)
     {
      d_pitIdOfCurrentCatchment = pitIdOfCatchment;
     }

     void     finishVertex(size_t v)
     {
       if (! pcr::isMV(d_flux[v]))
           d_flux[v] /= d_nrTimeSlices[d_pitIdOfCurrentCatchment];
     }

     DivideFluxByNrTimeSlices(
          Field & flux,
          VField<INT4>  const& nrTimeSlices,
          LddGraph      const& lg):

         DownstreamVisitor(lg),
         d_flux(flux.dest_f()),
         d_nrTimeSlices(nrTimeSlices)
       {
       }
  };

  ExecArguments arg(op,rte,nrArgs);
  ScopedLddGraph lgs(rte,arg[0]);
  LddGraph const& lg(lgs.current());

  arg.createResult();

  Field& newState(arg.result(0));
  Field& flux    (arg.result(1));

  const VField<INT4> nrTimeSlices(arg[5],newState.nrValues());

  KWFSVisitor v(newState,flux,nrTimeSlices ,arg[6],arg, lg);
  v.visitPerCachmentSlice();
  v.computeState();

  DivideFluxByNrTimeSlices df(flux,nrTimeSlices, lg);
  df.visitEntireLdd();

  arg.pushResults();
}


#define MAX_ITERS 3000

static double iterateToQnew(
    double Qin,   /* summed Q new in for all sub-catchments */
    double Qold,  /* current discharge */
    double q,
    double alpha,
    double beta,
    double sliceInSecs,
    double deltaX)
{
    const  double epsilon(1E-12); // iteration epsilon

    // Using Newton-Raphson Method
    typedef long double REAL;
    REAL Qk1;      // Q at loop k+1 for i+1, j+1
    REAL ab_pQ, deltaTX, C;
    int   count;

    REAL Qkx;
    REAL fQkx;
    REAL dfQkx;
    POSTCOND(sizeof(REAL) >= 8);

    // if no input then output = 0
    if ((Qin+Qold+q) == 0)  // +q CW NEW!
        return(0);

    // common terms
    ab_pQ = alpha*beta*std::pow(((Qold+Qin)/2),beta-1);
    deltaTX = sliceInSecs/deltaX;
    C = deltaTX*Qin + alpha*std::pow(Qold,beta) + sliceInSecs*q;

    //  1. Initial guess Qk1.
    //  2. Evaluate function f at Qkx.
    //  3. Evaluate derivative df at Qkx.
    //  4. Check convergence.

    // There's a problem with the first guess of Qkx. fQkx is only defined
    // for Qkx's > 0. Sometimes the first guess results in a Qkx+1 which is
    // negative or 0. In that case we change Qkx+1 to 1e-30. This keeps the
    // convergence loop healthy.
    Qkx   = (deltaTX * Qin + Qold * ab_pQ + sliceInSecs * q) / (deltaTX + ab_pQ);
    Qkx   = MAX(Qkx, 1e-30); /* added test-case calc::KinematicTest::iterate1 */
    fQkx  = deltaTX * Qkx + alpha * std::pow(Qkx, (REAL)beta) - C;   /* Current k */
    dfQkx = deltaTX + alpha * beta * std::pow(Qkx, (REAL)beta - 1);  /* Current k */
    Qkx   -= fQkx / dfQkx;                                /* Next k */
    Qkx   = MAX(Qkx, 1e-30);
    count = 0;
    do {
      fQkx  = deltaTX * Qkx + alpha * std::pow(Qkx, (REAL)beta) - C;   /* Current k */
      dfQkx = deltaTX + alpha * beta * std::pow(Qkx, (REAL)beta - 1);  /* Current k */
      Qkx   -= fQkx / dfQkx;                                /* Next k */
      Qkx   = MAX(Qkx, 1e-30);
      count++;
    } while(std::fabs(fQkx) > epsilon && count < MAX_ITERS);

#ifdef DEBUG_DEVELOP
    /* Our loop should converge in around 2 steps, otherwise something's
     * wrong.
     */
    /*  test-case calc::KinematicTest::iterate2
     *  is such a case, but values are very low
     * 1e-30 is returned
     */
     // if (count == MAX_ITERS) {
     //  printf("\nfQkx %g Qkx %g\n",(double)fQkx, (double)Qkx);
     //  printf("Qin %g \n", Qin);
     //  printf("Qold %g \n", Qold);
     //  printf("q %g \n", q);
     //  printf("alpha %g \n",alpha);
     //  printf("beta %g \n", beta);
     //  printf("sliceInSecs %g \n", sliceInSecs);
     //  printf("deltaX %g \n", deltaX);
     // }
#endif
    Qk1 = Qkx;
    return (double)(MAX(Qk1,0));
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



