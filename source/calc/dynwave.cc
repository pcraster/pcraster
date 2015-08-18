#include "stddefx.h"

// vim: fileformat=dos
// Others need it


#include "calc.h"  // for it's own interface
#ifndef INCLUDED_APPARGS
#include "appargs.h"      // appDynamicWaveRoughness
#define INCLUDED_APPARGS
#endif
#ifndef INCLUDED_COM_MATH
#include "com_math.h"
#define INCLUDED_COM_MATH
#endif
#ifndef INCLUDED_MISC
#include "misc.h"         // RetError
#define INCLUDED_MISC
#endif
#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
#endif


#ifndef INCLUDED_GEO_CELLLOCVISITOR
#include "geo_celllocvisitor.h"
#define INCLUDED_GEO_CELLLOCVISITOR
#endif

#ifndef INCLUDED_FIELDAPI_INTERFACE
#include "fieldapi_interface.h"
#define INCLUDED_FIELDAPI_INTERFACE
#endif
#ifndef INCLUDED_FIELDAPI_SCALARDOMAINCHECK
#include "fieldapi_scalardomaincheck.h"
#define INCLUDED_FIELDAPI_SCALARDOMAINCHECK
#endif

#ifndef INCLUDED_CALC_DOWNSTREAMVISITOR
#include "calc_downstreamvisitor.h"
#define INCLUDED_CALC_DOWNSTREAMVISITOR
#endif

#ifndef INCLUDED_COM_INTERVALTYPES
#include "com_intervaltypes.h"
#define INCLUDED_COM_INTERVALTYPES
#endif

static double AactToLevel(
    double Aact, double Bw,
    double yc, double z,
    double FW)
{
  double Acmax=(Bw+z*yc)*yc;
  if (Aact > Acmax)
    return yc + (Aact - Acmax)/FW;
  if (z > 0)
    return (-Bw + std::sqrt(com::pow2(Bw)+4*z*Aact))/(2*z); // abc-formula
  return Aact/Bw;
}

static double LevelToAact(
    double y, double Bw,
    double yc, double z,
    double FW)
{
  if (y < yc)
    return (Bw + z*y)*y;
  return (Bw+z*yc)*yc + (y-yc)*FW;
}

static double LevelToAactStream(
    double y, double Bw, double z)
{
  return (Bw+z*y)*y;
}

/*
 * static double LevelToP(
 *   double y, double Bw,
 *   double yc, double z,
 *   double FW)
 *{
 * if (y <= yc)
 *   return Bw + 2* y*std::sqrt(1+com::pow2(z));
 * return Bw+2*yc*std::sqrt(1+com::pow2(z))+(FW-Bw-yc*z)+2*(y-yc);
 *}
 */

static double LevelToPStream(
    double y, double Bw, double z)
{
  return Bw + 2* y*std::sqrt(1+com::pow2(z));
}

inline static double sqrtSf(
    double Sf)
{
  if (Sf >= 0)
        return std::sqrt(Sf);
  return -std::sqrt(-Sf);
}

//! calculates Q in m3/sec
static double QDynChezy(
    double Roughness,
    double Aact,
    double Sf,
    double R)
{
  // Roughness is Chezy-coefficient
  return Roughness  * Aact * sqrtSf(Sf)  * std::sqrt(R);
}

//! calculates Q in m3/sec
static double QDynManning(
    double Roughness,
    double Aact,
    double Sf,
    double R)
{
  // Roughness is Manning coefficient
   return (sqrtSf(Sf) * Aact * std::pow(R, 2.0 / 3.0)) / Roughness;
}

//! calculates Q in m3/sec
static double QStructure(
  double resultH,
  double structureA,
  double structureB,
  double structureCrestLevel)
{
  if (resultH > structureCrestLevel)
    return structureA*std::pow(resultH-structureCrestLevel,structureB);
  return 0;
}


/*!
   call as
    q,h = dynamicwaveq,dynamicwaveh(ldd, ...etc );

   WISHLIST: short version, no structures: is the same as with structures
   false everywhere.

   Global options, --manning (default) --chezy
    sets if the channelRoughness parameter defines
    the Chezy- or the Manning-coefficient

   addditonal symbols, not listed in argument list
    *Volume = volumes       (m3)
    y       = water level,H (m)

 */
extern "C" int DynamicWave(
 MAP_REAL8 *m_resultQ,                  // scalar, new Q      [  , m3/timeStep]
 MAP_REAL8 *m_resultH,                  // scalar, new H      [  , m ]
 const MAP_UINT1 *m_ldd,                // ldd
 const MAP_REAL8 *m_Qin,                // scalar             [  , m3/timeStep]
 const MAP_REAL8 *m_Hin,                // scalar, >= 0       [  , m ]
 const MAP_REAL8 *m_channelBottomLevel, // scalar
 const MAP_REAL8 *m_channelRoughness,   // scalar, > 0
                                        //  if Chezy-coefficient [C, m1/2sec-1]
                                        //  if Manning-coeff     [Km, m1/3sec-1]
 const MAP_REAL8 *m_channelLength,      // scalar, > 0        [L , m]
 const MAP_REAL8 *m_channelBottomWidth, // scalar, >= 0       [Bw, m]
 const MAP_REAL8 *m_channelDepth,       // scalar, >= 0       [yc, m]
 const MAP_REAL8 *m_channelForm,        // scalar, >= 0       [z , -]
 const MAP_REAL8 *m_floodplainWidth,    // scalar,            [FW, m]
                                        //         >= Bw+yc+z
 const MAP_REAL8 *m_timeStepInSeconds,  // scalar, nonspatial > 0  [ sec ]
 const MAP_REAL8 *m_nrTimeSlices,       // scalar, nonspatial > 0
 const MAP_UINT1 *m_structures,         // boolean
 const MAP_REAL8 *m_structureA,         // scalar,
                                        //  active where structures is true
 const MAP_REAL8 *m_structureB,         // scalar,
                                        //  active where structures is true
 const MAP_REAL8 *m_structureCrestLevel)// scalar,
                                        //  active where structures is true
{
  ReadWriteReal8_ref(resultQ,m_resultQ);
  ReadWriteReal8_ref(resultH,m_resultH);

  std::vector<const fieldapi::Common*>     inputs;
  ReadOnlyUint1_ref(ldd,m_ldd); inputs.push_back(&ldd);
  ReadOnlyReal8_ref(Qin,m_Qin); inputs.push_back(&Qin);
  ReadOnlyReal8_ref(Hin,m_Hin); inputs.push_back(&Hin);
  ReadOnlyReal8_ref(channelBottomLevel,m_channelBottomLevel); 
    inputs.push_back(&channelBottomLevel);
  ReadOnlyReal8_ref(channelRoughness,m_channelRoughness); 
    inputs.push_back(&channelRoughness);
  ReadOnlyReal8_ref(L,m_channelLength); inputs.push_back(&L);
  ReadOnlyReal8_ref(Bw,m_channelBottomWidth); inputs.push_back(&Bw);
  ReadOnlyReal8_ref(yc,m_channelDepth); inputs.push_back(&yc);
  ReadOnlyReal8_ref(z,m_channelForm); inputs.push_back(&z);
  ReadOnlyReal8_ref(FW,m_floodplainWidth); inputs.push_back(&FW);

  ReadOnlyReal8_ref(timeStepInSecondsInterface,m_timeStepInSeconds);
  POSTCOND(!timeStepInSecondsInterface.spatial());
  double timeStepInSeconds=timeStepInSecondsInterface.value(0,0);

  ReadOnlyReal8_ref(nrTimeSlicesInterface,m_nrTimeSlices);
  POSTCOND(!nrTimeSlicesInterface.spatial());
  size_t nrTimeSlices = static_cast<size_t>(nrTimeSlicesInterface.value(0,0));

  // non spatial domains, check once
  std::vector<fieldapi::ScalarDomainCheck> domains,nsDomains;
  domains.push_back(fieldapi::ScalarDomainCheck(Hin,"Hin",
                                               com::GreaterThanEqualTo<double>(0)));
  domains.push_back(fieldapi::ScalarDomainCheck(channelRoughness,
                           "ChannelRoughness", com::GreaterThan<double>(0)));
  domains.push_back(fieldapi::ScalarDomainCheck(L,"ChannelLength",
                                               com::GreaterThan<double>(0)));
  domains.push_back(fieldapi::ScalarDomainCheck(Bw,"ChannelBottomWidth",
                                               com::GreaterThanEqualTo<double>(0)));
  domains.push_back(fieldapi::ScalarDomainCheck(yc,"ChannelDepth",
                                               com::GreaterThanEqualTo<double>(0)));
  domains.push_back(fieldapi::ScalarDomainCheck(z,"ChannelForm",
                                               com::GreaterThanEqualTo<double>(0)));

  // floodplainWidth (FW) is special case, done in processing loop
  nsDomains.push_back(fieldapi::ScalarDomainCheck(timeStepInSecondsInterface,
        "TimeStepInSeconds", com::GreaterThan<double>(0)));
  nsDomains.push_back(fieldapi::ScalarDomainCheck(nrTimeSlicesInterface,
        "NrTimeSlices", com::GreaterThan<double>(0)));

  int nsCheck = fieldapi::checkScalarDomains(nsDomains,geo::CellLoc(0,0));
  if (nsCheck != -1)// pcrcalc/test350
    return RetError(1,nsDomains[nsCheck].msg().c_str());

  ReadOnlyUint1_ref(structures,m_structures);
   inputs.push_back(&structures);
  ReadOnlyReal8_ref(structureA,m_structureA);
   inputs.push_back(&structureA);
  ReadOnlyReal8_ref(structureB,m_structureB);
   inputs.push_back(&structureB);
  ReadOnlyReal8_ref(structureCrestLevel,m_structureCrestLevel);
   inputs.push_back(&structureCrestLevel);

  // select the correct equation
  double (*qDyn)(double Roughness, double Aact, double Sf, double R) =
    (appDynamicWaveRoughness==APP_DWR_CHEZY) ? QDynChezy : QDynManning;

  // find all catchmentOutlets
  // init
  //   resultQ=0
  //   resultH=Hin
  // mark resultQ with MV if ANY of the inputs is MV
  std::vector<geo::CellLoc> catchmentOutlets;
  for(geo::CellLocVisitor cl(ldd); cl.valid(); ++cl) {
     geo::CellLoc c(*cl);
     UINT1 cVal;
     if ( (ldd.get(cVal,c)) && cVal == LDD_PIT) // found a catchment outlet */
         catchmentOutlets.push_back(c);
     if (fieldapi::nonMV(inputs,c)) {
       resultQ.put(0,c);
       resultH.copy(Hin,c);
     } else {
       // MV on some inputs is MV on result
       resultQ.putMV(c);
       resultH.putMV(c);
     }
  }

  double iterationTime= timeStepInSeconds/nrTimeSlices; //[sec]

  for(size_t slice=0;slice<nrTimeSlices;slice++)
  for(size_t catchment=0; catchment<catchmentOutlets.size(); catchment++)
  // WPA 2.1
  for(calc::DownStreamVisitor v(ldd,catchmentOutlets[catchment]);v.valid();++v){
    geo::CellLoc  c=*v; // current cell
    if (resultQ.isMV(c)) // marked as MV if some inputs is MV
      break;

    if (slice == 0) { // check once

      int check = fieldapi::checkScalarDomains(domains,c);
      if (check != -1)
        return RetError(1,domains[check].msg().c_str()); // pcrcalc/test349
      if (!(FW[c] >= (Bw[c]+yc[c]*z[c])))
        return RetError(1, // pcrcalc/test351
          "FloodplainWidth not wider than bankfull channel width");
    }

    geo::LDD::Code cL=ldd[c]; // ldd value of c

    double QtimeStep; // Q  m3/sec
    // WPA 2.3 is  cell a structure or channel segment
    if (structures[c] == 1)
     QtimeStep= iterationTime *
                QStructure(resultH[c], structureA[c], structureB[c],
                                       structureCrestLevel[c]);
    else {
      if (cL == LDD_PIT) {
         QtimeStep=0; // no downstream cell
      } else { // has downstream cell
        // WPA 2.2
        geo::CellLoc ds=geo::LDD::target(c,cL);
        if (resultQ.isMV(ds)) // marked as MV if some inputs is MV
            break;

        if (resultH[c] <= 0)
          QtimeStep = 0;
        else {
          double levToAact=LevelToAactStream(resultH[c], Bw[c], z[c]);
          double levToP   =   LevelToPStream(resultH[c], Bw[c], z[c]);
          double Sf    = ((resultH[c]+channelBottomLevel[c])-
                         (resultH[ds]+channelBottomLevel[ds]))/L[c];
          QtimeStep = iterationTime * qDyn(channelRoughness[c],
                          levToAact, Sf,
                          /* R = */ levToAact / levToP);
        }
     }
    }

    // 2.4 new volumes
    double cVolume=L[c] *
                   LevelToAact(resultH[c], Bw[c], yc[c],z[c], FW[c]);
    cVolume+=Qin[c]/nrTimeSlices; 

    double dsVolume;
    if (cL == LDD_PIT) {
         dsVolume=0; // no downstream cell
    } else { // has downstream cell
     geo::CellLoc ds=geo::LDD::target(c,cL);
     dsVolume=L[ds] *
      LevelToAact(resultH[ds], Bw[ds],
          yc[ds],z[ds], FW[ds]);
    }

    if (QtimeStep > 0)
      QtimeStep=std::min(QtimeStep,cVolume);
    else
      QtimeStep=std::max(QtimeStep,-dsVolume);

    cVolume-=QtimeStep;

    resultH.put(AactToLevel(cVolume/L[c], Bw[c], yc[c], z[c],FW[c]),c);
    resultQ.put(resultQ[c]+QtimeStep,c);

    if (cL != LDD_PIT) {
     // WPA 2.4.1 change resultH[downstream]
     geo::CellLoc ds=geo::LDD::target(c,cL);
     dsVolume+=QtimeStep;
     resultH.put(AactToLevel(dsVolume/L[ds], Bw[ds], yc[ds], z[ds],FW[ds]),ds);
    }
  }

  return 0;
}
