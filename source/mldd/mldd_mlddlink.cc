#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_MLDD_MLDDLINK
#include "mldd_mlddlink.h"
#define INCLUDED_MLDD_MLDDLINK
#endif

// Library headers.

// PCRaster library headers.
#ifndef INCLUDED_FIELDAPI_INTERFACE
#include "fieldapi_interface.h"
#define INCLUDED_FIELDAPI_INTERFACE
#endif

// Module headers.
#ifndef INCLUDED_MLDD_MLDD
#include "mldd_mldd.h"
#define INCLUDED_MLDD_MLDD
#endif

/*!
  \file
  This file contains the implementation of the MlddLink class.
*/



//------------------------------------------------------------------------------

/*! \todo
 *   dit eigenlijk const terug, maak duidelijk onderscheid tussen in's en 
 *   out's op const nivo in neue interface
 */
static REAL4* inCast(const calc::ModelLinkArgSpec& a)
{
  PRECOND(a.cr==PCR_CR_REAL8);
  PRECOND(a.st==PCR_ST_SPATIAL);
  return  (REAL4 *)
    (static_cast<const MAP_REAL8 * >(a.value)->spatialValue[0]);
}

static UINT1* inCast1(const calc::ModelLinkArgSpec& a)
{
  PRECOND(a.cr==PCR_CR_UINT1);
  PRECOND(a.st==PCR_ST_SPATIAL);
  return  (UINT1 *)
    (static_cast<const MAP_UINT1 * >(a.value)->spatialValue[0]);
}

static REAL4* outCast(const calc::ModelLinkArgSpec& a)
{
  PRECOND(a.cr==PCR_CR_REAL8);
  PRECOND(a.st==PCR_ST_SPATIAL);
  return  (REAL4 *)
    (static_cast<MAP_REAL8 * >(a.value)->spatialValue[0]);
}

static UINT1* outCast1(const calc::ModelLinkArgSpec& a)
{
  PRECOND(a.cr==PCR_CR_UINT1);
  PRECOND(a.st==PCR_ST_SPATIAL);
  return  (UINT1 *)
    (static_cast<MAP_UINT1 * >(a.value)->spatialValue[0]);
}

//------------------------------------------------------------------------------
// DEFINITION OF STATIC MLDDLINK MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF MLDDLINK MEMBERS
//------------------------------------------------------------------------------

mldd::MlddLink::MlddLink()
  : calc::ModelLink(),
    d_mldd(0)
{
}



mldd::MlddLink::~MlddLink()
{
  delete d_mldd;
}



const std::string& mldd::MlddLink::name() const
{
  static const std::string name = "mldd";
  return name;
}

#define LINK_CHECK_DISPATCH(name) \
  if(methodName == # name ) {     \
    name##Check(sig);             \
    return true;                  \
  }

#define LINK_EXEC_DISPATCH(name) \
  if(methodName == # name ) {    \
    name##Execute(sig);          \
    return;                      \
  }


bool mldd::MlddLink::methodCheck(const std::string& methodName,
                   Sig& sig) const
{
  LINK_CHECK_DISPATCH(addStream);
  LINK_CHECK_DISPATCH(setStream);
  LINK_CHECK_DISPATCH(getStream);
  LINK_CHECK_DISPATCH(removeStream);
  LINK_CHECK_DISPATCH(getWeight);
  LINK_CHECK_DISPATCH(setDem);
  LINK_CHECK_DISPATCH(getDem);
  LINK_CHECK_DISPATCH(upstream);
  LINK_CHECK_DISPATCH(accuflux);

  LINK_CHECK_DISPATCH(diffuse);
  return false;
}



void mldd::MlddLink::methodExecute(const std::string& methodName,
                  Sig& sig)
{
  LINK_EXEC_DISPATCH(addStream);
  LINK_EXEC_DISPATCH(setStream);
  LINK_EXEC_DISPATCH(getStream);
  LINK_EXEC_DISPATCH(removeStream);
  LINK_EXEC_DISPATCH(getWeight);
  LINK_EXEC_DISPATCH(setDem);
  LINK_EXEC_DISPATCH(getDem);
  LINK_EXEC_DISPATCH(upstream);
  LINK_EXEC_DISPATCH(accuflux);

  LINK_EXEC_DISPATCH(diffuse);

  // This only happens if check is implemented, but execute is not.
  throw calc::ModelLinkException("unknown method");
}



void mldd::MlddLink::initCheck(Sig& sig) const
{
  // Configure input args.
  sig.d_input.resize(0);

/*
  // Argument 1
  sig.d_input[0].st = PCR_ST_NONSPATIAL;
  sig.d_input[0].vs = PCR_VS_S;
  sig.d_input[0].cr = PCR_CR_REAL8;
*/

}

void mldd::MlddLink::initExecute(
  const geo::RasterSpace& rs,
  Sig& sig)
{
  PRECOND(sig.d_input.size() == 0);
  PRECOND(!d_mldd);
  d_mldd = new Mldd(rs);
}

void mldd::MlddLink::addStreamCheck(Sig& sig) const
{
  // Configure input args.
  sig.d_input.resize(1);

  // ldd
  sig.d_input[0].st = PCR_ST_SPATIAL;
  sig.d_input[0].vs = PCR_VS_L;
  sig.d_input[0].cr = PCR_CR_UINT1;

  // Configure output.
  sig.d_result.resize(1);

  // result is a boolean if successful?
  sig.d_result[0].st = PCR_ST_SPATIAL;
  sig.d_result[0].vs = PCR_VS_B;
  sig.d_result[0].cr = PCR_CR_UINT1;
}

void mldd::MlddLink::addStreamExecute(Sig& sig) const
{
  PRECOND(sig.d_input.size() == 1);
  PRECOND(sig.d_input[0].cr == PCR_CR_UINT1);

  d_mldd->addStream(inCast1(sig.d_input[0]));
}

void mldd::MlddLink::setStreamCheck(Sig& sig) const
{
  // Configure input args.
  sig.d_input.resize(8);

  // ldd
  sig.d_input[0].st = PCR_ST_SPATIAL;
  sig.d_input[0].vs = PCR_VS_L;
  sig.d_input[0].cr = PCR_CR_UINT1;
  for(size_t i=1;i<8;++i)
   sig.d_input[i]=sig.d_input[0];

  // Configure output.
  sig.d_result.resize(1);

  // result is a boolean if successful?
  sig.d_result[0].st = PCR_ST_SPATIAL;
  sig.d_result[0].vs = PCR_VS_B;
  sig.d_result[0].cr = PCR_CR_UINT1;
}

void mldd::MlddLink::setStreamExecute(Sig& sig) const
{
  PRECOND(sig.d_input.size() == 8);

  std::vector<const UINT1 *> input;
  for(size_t r=0;r<8; r++)
    input.push_back(inCast1(sig.d_input[r]));

  d_mldd->setStream(input);
}

void mldd::MlddLink::getStreamCheck(Sig& sig) const
{
  sig.d_input.resize(0);
  sig.d_result.resize(8);

  // ldd
  sig.d_result[0].st = PCR_ST_SPATIAL;
  sig.d_result[0].vs = PCR_VS_L;
  sig.d_result[0].cr = PCR_CR_UINT1;

  for(size_t i=1;i<8;++i)
   sig.d_result[i]=sig.d_result[0];
}

void mldd::MlddLink::getStreamExecute(Sig& sig) const
{
  PRECOND(sig.d_result.size() == 8);
  PRECOND(sig.d_input.size()  == 0);

  std::vector<UINT1 *> result;
  for(size_t r=0;r<8; r++)
    result.push_back(outCast1(sig.d_result[r]));
  d_mldd->getStream(result);
}

void mldd::MlddLink::getWeightCheck(Sig& sig) const
{
  sig.d_input.resize(0);
  sig.d_result.resize(8);

  // weights
  sig.d_result[0].st = PCR_ST_SPATIAL;
  sig.d_result[0].vs = PCR_VS_S;
  sig.d_result[0].cr = PCR_CR_REAL8;

  for(size_t i=1;i<8;++i)
   sig.d_result[i]=sig.d_result[0];
}

void mldd::MlddLink::getWeightExecute(Sig& sig) const
{
  PRECOND(sig.d_result.size() == 8);
  PRECOND(sig.d_input.size()  == 0);

  std::vector<REAL4 *> out;
  for(size_t r=0;r<8; r++)
    out.push_back(outCast(sig.d_result[r]));
  d_mldd->getWeight(out);
}

void mldd::MlddLink::setDemCheck(Sig& sig) const
{
  // Configure input args.
  sig.d_input.resize(1);

  // ldd
  sig.d_input[0].st = PCR_ST_SPATIAL;
  sig.d_input[0].vs = PCR_VS_S;
  sig.d_input[0].cr = PCR_CR_REAL8;

  // Configure bogus output.
  sig.d_result.resize(1);
  // result is a boolean if successful?
  sig.d_result[0].st = PCR_ST_SPATIAL;
  sig.d_result[0].vs = PCR_VS_B;
  sig.d_result[0].cr = PCR_CR_UINT1;

}

void mldd::MlddLink::setDemExecute(Sig& sig) const
{
  PRECOND(sig.d_input.size() == 1);
  PRECOND(sig.d_input[0].cr == PCR_CR_REAL8);

  const REAL4 *newDem=inCast(sig.d_input[0]);
  d_mldd->setDem(newDem);
}

void mldd::MlddLink::getDemExecute(Sig& sig) const
{
  PRECOND(sig.d_result.size() == 1);
  PRECOND(sig.d_input.size()  == 0);

  d_mldd->getDem(outCast(sig.d_result[0]));
}

void mldd::MlddLink::getDemCheck(Sig& sig) const
{
  sig.d_input.resize(0);
  sig.d_result.resize(1);

  sig.d_result[0].st = PCR_ST_SPATIAL;
  sig.d_result[0].vs = PCR_VS_S;
  sig.d_result[0].cr = PCR_CR_REAL8;
}

void mldd::MlddLink::upstreamCheck(Sig& sig) const
{
  // amount
  sig.d_input.resize(1);
  sig.d_input[0].st = PCR_ST_SPATIAL;
  sig.d_input[0].vs = PCR_VS_S;
  sig.d_input[0].cr = PCR_CR_REAL8;

  sig.d_result.resize(1);
  sig.d_result[0] = sig.d_input[0];
}


void mldd::MlddLink::upstreamExecute(Sig& sig) const
{
  PRECOND(sig.d_input.size() == 1);
  PRECOND(sig.d_input[0].cr == PCR_CR_REAL8);
  PRECOND(sig.d_result.size() == 1);
  PRECOND(sig.d_result[0].cr == PCR_CR_REAL8);

  d_mldd->upstream(
        outCast(sig.d_result[0]),
        inCast(sig.d_input[0]));
}

void mldd::MlddLink::accufluxCheck(Sig& sig) const
{
  // amount
  sig.d_input.resize(1);
  sig.d_input[0].st = PCR_ST_SPATIAL;
  sig.d_input[0].vs = PCR_VS_S;
  sig.d_input[0].cr = PCR_CR_REAL8;

  sig.d_result.resize(1);
  sig.d_result[0] = sig.d_input[0];
}


void mldd::MlddLink::accufluxExecute(Sig& sig) const
{
  PRECOND(sig.d_input.size() == 1);
  PRECOND(sig.d_input[0].cr == PCR_CR_REAL8);
  PRECOND(sig.d_result.size() == 1);
  PRECOND(sig.d_result[0].cr == PCR_CR_REAL8);

  d_mldd->accuflux(outCast(sig.d_result[0]), inCast(sig.d_input[0]));
}

void mldd::MlddLink::diffuseCheck(Sig& sig) const
{
  sig.d_input.resize(12);
  // 0) Input/State
  sig.d_input[0].st = PCR_ST_SPATIAL;
  sig.d_input[0].vs = PCR_VS_S;
  sig.d_input[0].cr = PCR_CR_REAL8;
  // 1) Area
  // 2) Fixed Head (Eigenlijk boolean)
  // 3-10) DiffusionValue
  for(size_t i=1;i<11;++i)
    sig.d_input[i] = sig.d_input[0];
  // 11) iterations
  sig.d_input[11].st = PCR_ST_NONSPATIAL;
  sig.d_input[11].vs = PCR_VS_N;
  sig.d_input[11].cr = PCR_CR_INT4;

  // totalOutflow
  sig.d_result.resize(1);
  sig.d_result[0].st = PCR_ST_SPATIAL;
  sig.d_result[0].vs = PCR_VS_S;
  sig.d_result[0].cr = PCR_CR_REAL8;

}

void mldd::MlddLink::diffuseExecute(Sig& sig) const
{
  PRECOND(sig.d_input.size() == 12);
  for(size_t i=0; i<11;++i) {
   PRECOND(sig.d_input[i].cr == PCR_CR_REAL8);
   PRECOND(sig.d_input[i].st == PCR_ST_SPATIAL);
  }

  PRECOND(sig.d_input[11].cr == PCR_CR_INT4);
  PRECOND(sig.d_input[11].st == PCR_ST_NONSPATIAL);

  PRECOND(sig.d_result.size() == 1);
  PRECOND(sig.d_result[0].st == PCR_ST_SPATIAL);
  PRECOND(sig.d_result[0].cr == PCR_CR_REAL8);

  std::vector<const REAL4 *> diffusionValueInArgOrder;
  for(size_t i=3; i < 11; ++i)
   diffusionValueInArgOrder.push_back(inCast(sig.d_input[i]));
  INT4 nrIterations=
   (static_cast<const MAP_INT4 * >(sig.d_input[11].value))->nonSpatialValue;

  d_mldd->diffuse(outCast(sig.d_result[0]),
                   inCast(sig.d_input[0]), // oldState
                   inCast(sig.d_input[1]), // area
                   inCast(sig.d_input[2]), // fixedHead TODO boolean
                   diffusionValueInArgOrder,
                   nrIterations);
}

void mldd::MlddLink::removeStreamCheck(Sig& sig) const
{
  sig.d_input.resize(8);
  for(size_t i=0;i<8;++i) {
   sig.d_input[i].st = PCR_ST_SPATIAL;
   sig.d_input[i].vs = PCR_VS_L;
   sig.d_input[i].cr = PCR_CR_UINT1;
  }

  // Configure bogus output.
  sig.d_result.resize(1);
  // result is a boolean if successful?
  sig.d_result[0].st = PCR_ST_SPATIAL;
  sig.d_result[0].vs = PCR_VS_B;
  sig.d_result[0].cr = PCR_CR_UINT1;
}

void mldd::MlddLink::removeStreamExecute(Sig& sig) const
{
  PRECOND(sig.d_input.size()==8);

  std::vector<const UINT1 *> marksInArgOrder;
  for(size_t i=0;i<8;++i)
   marksInArgOrder.push_back(inCast1(sig.d_input[i]));
  d_mldd->removeStream(marksInArgOrder);
}


//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------
