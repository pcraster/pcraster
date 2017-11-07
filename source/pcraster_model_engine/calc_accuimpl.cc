#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_ACCUIMPL
#include "calc_accuimpl.h"
#define INCLUDED_CALC_ACCUIMPL
#endif

// Library headers.
#ifndef INCLUDED_CMATH
#include <cmath>
#define INCLUDED_CMATH
#endif
#ifndef INCLUDED_BOOST_NONCOPYABLE
#include <boost/noncopyable.hpp>
#define INCLUDED_BOOST_NONCOPYABLE
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
// Module headers.
#ifndef INCLUDED_CALC_EXECARGUMENTS
#include "calc_execarguments.h"
#define INCLUDED_CALC_EXECARGUMENTS
#endif
#ifndef INCLUDED_CALC_DOMAINERROR
#include "calc_domainerror.h"
#define INCLUDED_CALC_DOMAINERROR
#endif
#ifndef INCLUDED_CALC_DOWNSTREAMVISITOR
#include "calc_downstreamvisitor.h"
#define INCLUDED_CALC_DOWNSTREAMVISITOR
#endif
#ifndef INCLUDED_CALC_VFIELD
#include "calc_vfield.h"
#define INCLUDED_CALC_VFIELD
#endif
#ifndef INCLUDED_CALC_SCOPEDLDDGRAPH
#include "calc_scopedlddgraph.h"
#define INCLUDED_CALC_SCOPEDLDDGRAPH
#endif
#ifndef INCLUDED_CALC_LDDGRAPH
#include "calc_lddgraph.h"
#define INCLUDED_CALC_LDDGRAPH
#endif
#ifndef INCLUDED_CALC_FIELD
#include "calc_field.h"
#define INCLUDED_CALC_FIELD
#endif

/*!
  \file
  This file contains the implementation of the AccuImpl class.
*/



//------------------------------------------------------------------------------

namespace calc {

//! function object for AccuImpl
struct AccumulateFlux {
  // virtual ~AccumulateFlux() {};
  virtual float operator()(float   interimState,
                           size_t  pos) const=0;
  virtual bool mv(size_t /* pos */) const
  { return false; }
};

//! function object for AccuImpl with 1 parameter
struct AccumulateFluxPar:
  public AccumulateFlux
  // , public boost::noncopyable, if enable results in pure virtual function
  // called on linux/gcc
{
  const VField<float>& d_par;
  AccumulateFluxPar(const VField<float>& par):
    d_par(par)
    {}
  virtual bool mv(size_t pos) const {
    return pcr::isMV(d_par[pos]);
  }
};


AccuAll accuAll;

class Capacity : public AccumulateFluxPar {
public:
  Capacity(const VField<float>& capacity):
    AccumulateFluxPar(capacity)
    {}
  float operator()(float interimState, size_t pos) const {
    return std::min(interimState,d_par[pos]);
  }
};
AccuCapacity accuCapacity;

class Threshold : public AccumulateFluxPar {
public:
  Threshold(const VField<float>& threshold):
    AccumulateFluxPar(threshold)
    {}
  float operator()(float interimState, size_t pos) const {
    float threshold=d_par[pos];
    if (threshold < 0)
      throw DomainError("threshold < 0");
    if(interimState <= threshold)
       return 0;
    return interimState - threshold;
  }
};
AccuThreshold accuThreshold;

class Fraction : public AccumulateFluxPar {
public:
  Fraction(const VField<float>& fraction):
    AccumulateFluxPar(fraction)
    {}
  float operator()(float interimState, size_t pos) const {
    float fraction=d_par[pos];
    if (0 > fraction || fraction > 1)
      throw DomainError("fraction not in [0,1] range");
    return interimState * fraction;
  }
};
AccuFraction accuFraction;

class Trigger : public AccumulateFluxPar {
public:
  Trigger(const VField<float>& trigger):
    AccumulateFluxPar(trigger)
    {}
  float operator()(float interimState, size_t pos) const {
    float trigger=d_par[pos];
    if (trigger < 0)
      throw DomainError("trigger < 0");
    if (trigger <= interimState)
      return interimState;
    return 0;
  }
};
AccuTrigger accuTrigger;

//! Accumulation through a directed graph, like LddGraph
class AccuImpl : public DownstreamVisitor
{
private:
   AccuImpl&          operator=           (AccuImpl const& rhs);
                   AccuImpl               (AccuImpl const& rhs);

   //! the result state
   float*                 d_newState;
   //! the result flux
   float*                 d_flux;

   const  VField<float>&  d_oldState;
   const  AccumulateFlux& d_fluxFo;

   void visitEdge        (size_t up, size_t down);
   void finishVertex     (size_t v);

public:

                   AccuImpl               (float* newState, float* flux,
                                           const  LddGraph& lg,
                                           const  VField<float>&  oldState,
                                           const  AccumulateFlux& fluxFo);

  /* virtual */    ~AccuImpl              ();

};


template<class AccuStateFo>
static void accuStateFlux(
    RunTimeEnv*            rte,
    const Operator&        op)
  {
    ExecArguments arg(op,rte,3);
    arg.createResults();
    Field& newState(arg.result(0));
    Field& flux    (arg.result(1));

    size_t size(flux.nrValues());

    ScopedLddGraph      lg(rte,arg[0]);
    // TODO void calc::ExecutorTest::testRunTimeErrors()
    //  swap value of arg[1] and arg[2] error description is not
    //  correct it contains _mrf
    const VField<REAL4> oldState(arg[1],size);
    const VField<REAL4>      par(arg[2],size);

    AccuImpl ag(newState.dest_f(), flux.dest_f(),
                       lg.current(),oldState,AccuStateFo(par));
    ag.visitEntireLdd();


    arg.pushResults();
  }


} // namespace calc



//------------------------------------------------------------------------------
// DEFINITION OF STATIC ACCUIMPL MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF ACCUIMPL MEMBERS
//------------------------------------------------------------------------------

calc::AccuImpl::AccuImpl(
    float* newState, float* flux,
    const  LddGraph& lg,
    const  VField<float>&  oldState,
    const  AccumulateFlux& fluxFo):
     DownstreamVisitor(lg),
     d_newState(newState),
     d_flux(flux),
     d_oldState(oldState),
     d_fluxFo(fluxFo)
{
    graph().initField<float>(d_flux,0.0);
}

calc::AccuImpl::~AccuImpl()
{
}


/*
 * void calc::AccuImpl::discoverVertex(size_t v)
 * {
 * d_flux[v]=0;
 * }
 */

void calc::AccuImpl::visitEdge(size_t up, size_t down)
{
  // send calculated flux to down
  com::inplace_add(d_flux[down],d_flux[up]);
}

void calc::AccuImpl::finishVertex(size_t v)
{
  if (com::oneIsMV(d_oldState[v],d_flux[v])|d_fluxFo.mv(v)) {
    pcr::setMV(d_flux[v]);
    pcr::setMV(d_newState[v]);
  } else {
    // up has al its in-fluxes now summed in flux
    float interimState = d_oldState[v] + d_flux[v];

    float newFlux = d_fluxFo(interimState,v);
        d_flux[v] = newFlux;
    d_newState[v] = interimState-newFlux;
  }
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------


/* BE WARE \a op may NOT the op of the mrf, but they all
 * share the same args see OneOfMRF::exec
 */

#define ACCU_CLASS(Name)                                     \
void calc::Accu##Name::exec(                                 \
    RunTimeEnv* rte, const Operator& op,size_t nrArgs) const \
{ PRECOND(nrArgs==3); accuStateFlux<Name>(rte,op); }

ACCU_CLASS(Capacity)
ACCU_CLASS(Threshold)
ACCU_CLASS(Fraction)
ACCU_CLASS(Trigger)

void calc::AccuAll::exec(
    RunTimeEnv*     rte,
    const Operator& op, size_t) const
  {
     struct All : public AccumulateFlux {
       All() {}
     float operator()(float interimState, size_t /* pos */) const
      { return interimState; }
      ~All() {}
     };

    ExecArguments arg(op,rte,2);
    arg.createResult();
    Field& newState(arg.result(0));
    Field& flux    (arg.result(1));

    size_t size(flux.nrValues());

    ScopedLddGraph lg(rte,arg[0]);
    const VField<REAL4> oldState(arg[1],size);

    AccuImpl ag(newState.dest_f(), flux.dest_f(),lg.current(),oldState,All());
    ag.visitEntireLdd();

    arg.pushResults();
  }

