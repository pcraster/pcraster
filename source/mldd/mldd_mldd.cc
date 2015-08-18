#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_MLDD_MLDD
#include "mldd_mldd.h"
#define INCLUDED_MLDD_MLDD
#endif

// Library headers.
#ifndef INCLUDED_GEO_NEIGHBOUR
#include "geo_neighbour.h"
#define INCLUDED_GEO_NEIGHBOUR
#endif
// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_MLDD_DIFFUSE
#include "mldd_diffuse.h"
#define INCLUDED_MLDD_DIFFUSE
#endif
#ifndef INCLUDED_MLDD_ACCUFLUX
#include "mldd_accuflux.h"
#define INCLUDED_MLDD_ACCUFLUX
#endif
#ifndef INCLUDED_MLDD_DAGRASTER
#include "mldd_dagraster.h"
#define INCLUDED_MLDD_DAGRASTER
#endif
#ifndef INCLUDED_MLDD_WEIGHTMAP
#include "mldd_weightmap.h"
#define INCLUDED_MLDD_WEIGHTMAP
#endif
#ifndef INCLUDED_MLDD_UPSTREAM
#include "mldd_upstream.h"
#define INCLUDED_MLDD_UPSTREAM
#endif
#ifndef INCLUDED_MLDD_REMOVESTREAM
#include "mldd_removestream.h"
#define INCLUDED_MLDD_REMOVESTREAM
#endif



/*!
  \file
  This file contains the implementation of the Mldd class.
*/



//------------------------------------------------------------------------------

/*
namespace mldd {

class MlddPrivate
{
public:

  MlddPrivate()
  {
  }

  ~MlddPrivate()
  {
  }

};

} // namespace mldd
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC MLDD MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF MLDD MEMBERS
//------------------------------------------------------------------------------

//! old modellink
mldd::Mldd::Mldd(const geo::RasterSpace& rs):
  d_rs(rs),
  d_dag(new DagRaster(rs)),
  d_dem(new geo::ScalarSimpleRaster(rs))
{
}

/*
 * tricky "runtime back linking to app?"
//! new objectlink
mldd::Mldd::Mldd(calc::RunTimeEnv* rte):
  d_rs(rte->rasterSpace()),
  d_dag(new DagRaster(d_rs)),
  d_dem(new geo::ScalarSimpleRaster(d_rs))
{
}
*/



/* NOT IMPLEMENTED
//! Copy constructor.
mldd::Mldd::Mldd(Mldd const& rhs)

  : Base(rhs)

{
}
*/



mldd::Mldd::~Mldd()
{
}



/* NOT IMPLEMENTED
//! Assignment operator.
mldd::Mldd& mldd::Mldd::operator=(Mldd const& rhs)
{
  if (this != &rhs) {
  }
  return *this;
}
*/

void mldd::Mldd::addOneLdd(const UINT1 *ldd) const
{
  for(size_t i=0; i < d_rs.nrCells(); ++i) {
    UINT1 cVal=ldd[i];
    if ( cVal != MV_UINT1 && cVal != LDD_PIT)
       d_dag->addFlowNB(d_rs.convert(i),cVal);
  }
}

void mldd::Mldd::addStream(const UINT1* ldd)
{
  addOneLdd(ldd);

  try {
    d_dag->updateOrder();
  } catch(const NotADag& c) {
    throw com::Exception("Cycle detected in addStream\n");
  }
}

void mldd::Mldd::setStream(const std::vector<const UINT1*>& ldd)
{
  d_dag->clear();

  // order is not relevant for DagRaster code
  // since the ldd value itself is encoded within
  // a particular index
  for(size_t i=0; i < ldd.size(); ++i)
     addOneLdd(ldd[i]);

  try {
    d_dag->updateOrder();
  } catch(const NotADag& c) {
    throw com::Exception("Cycle detected in setStream\n");
  }
}

void mldd::Mldd::getWeight(
    std::vector<REAL4 *>& result) const
{
  PRECOND(result.size()==8);

  // order is relevant for python code
  // expected return order is N,NE,E,SE,S,SW,W,NW
  geo::LDD::Code lddCodes[8]={8,9 ,6,3 ,2,1 ,4,7 };

  WeightMap wm(*d_dag,*d_dem);

  for(size_t r=0;r<8; r++) {
    geo::NB::Code nb=geo::LDD::toNB(lddCodes[r]);
    wm.fillDirMap(nb,result[r]);
  }
}

void mldd::Mldd::getDem(
    REAL4 *dem) const
{
  std::copy(d_dem->begin(),d_dem->end(),dem);
}

void mldd::Mldd::setDem(
    const REAL4 *dem)
{
  std::copy(dem+0,dem+d_rs.nrCells(),d_dem->begin());
}

void mldd::Mldd::diffuse(REAL4*       totalOutflow,
                         const REAL4* oldState,
                         const REAL4* area,
                         const REAL4* fixedHead,
                         const std::vector<const REAL4 *>&
                           diffusionValueInArgOrder,
                         INT4         nrIterations)
{
  PRECOND(d_dem);
  Diffuse diffuse(*d_dem,
                  totalOutflow,
                  oldState,
                  area,
                  fixedHead,
                  diffusionValueInArgOrder,
                  nrIterations,
                  d_rs.cellSize());
  diffuse.run(*d_dag);
}

void mldd::Mldd::removeStream(const std::vector<const UINT1 *>& markInArgOrder)
{
  RemoveStream rsVisitor(*d_dag, markInArgOrder);
  d_dag->downstreamVisitor(rsVisitor);
  d_dag->remove(rsVisitor.removeDag());
}


void mldd::Mldd::upstream(
  REAL4*       out,
  const REAL4* in) const
{
  WeightMap wm(*d_dag,*d_dem);
  Upstream uv(wm,in,out);

  d_dag->downstreamVisitor(uv);
}

void mldd::Mldd::accuflux(
  REAL4*       out,
  const REAL4* in) const
{
  WeightMap wm(*d_dag,*d_dem);
  Accuflux uv(wm,in,out);

  d_dag->downstreamVisitor(uv);
}

void mldd::Mldd::getStream(
  const std::vector<UINT1 *>& result) const
{
  // order is relevant for python code
  // expected return order is
  //                         N,NE,E,SE,S,SW,W,NW
  geo::LDD::Code lddCodes[8]={8,9 ,6,3 ,2,1 ,4,7 };


  for(size_t r=0;r<8; r++) {
    UINT1 *out(result[r]);
    pcr::setMV(out,d_rs.nrCells());
    geo::NB::Code nb=geo::LDD::toNB(lddCodes[r]);
    for(size_t c=0; c < d_rs.nrCells(); c++) {
      if (d_dag->hasOutflowDir(c,nb))
        out[c]=lddCodes[r];
      if (d_dag->isPit(c))
        out[c]=5;
    }
  }
}



geo::RasterSpace const& mldd::Mldd::space() const
{
  return d_rs;
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



