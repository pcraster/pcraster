#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_MLDD_DIFFUSE
#include "mldd_diffuse.h"
#define INCLUDED_MLDD_DIFFUSE
#endif

// Library headers.
#ifndef INCLUDED_CMATH
#include <cmath>
#define INCLUDED_CMATH
#endif
#ifndef INCLUDED_STDEXCEPT
#include <stdexcept>
#define INCLUDED_STDEXCEPT
#endif
// PCRaster library headers.
#ifndef INCLUDED_COM_MVOP
#include "com_mvop.h"
#define INCLUDED_COM_MVOP
#endif
// Module headers.
#ifndef INCLUDED_MLDD_DOWNSTREAMVISITOR
#include "mldd_downstreamvisitor.h"
#define INCLUDED_MLDD_DOWNSTREAMVISITOR
#endif
#ifndef INCLUDED_MLDD_DAGRASTER
#include "mldd_dagraster.h"
#define INCLUDED_MLDD_DAGRASTER
#endif
#ifndef INCLUDED_MLDD_ARG2NBORDER
#include "mldd_arg2nborder.h"
#define INCLUDED_MLDD_ARG2NBORDER
#endif

/*!
  \file
  This file contains the implementation of the Diffuse class.
*/

//------------------------------------------------------------------------------

namespace mldd {

//! initialize diffusion iteration
class DiffuseInit : public DownstreamVisitor
{
  Diffuse& d_data;
  size_t   d_edgeVisitNr;
public:

  DiffuseInit(Diffuse& data, size_t nrEdges):
     DownstreamVisitor(data.d_dem.rasterDim()),
     d_data(data),
     d_edgeVisitNr(0)
  {
    d_data.d_fixedEdgeFlowTerm.reserve(nrEdges);
  }

  ~DiffuseInit()
  {
  }

  //! set d_totalOutflow to 0 or MV is any of the input is MV
  void initVertex(const Vertex& v) {
    d_data.initSet(linear(v));
  }
  void downstreamEdge(const Edge& e)
  {
    DEVELOP_PRECOND(d_edgeVisitNr < d_data.d_fixedEdgeFlowTerm.capacity());
    d_data.initFlowTerm(e);
    d_edgeVisitNr++;
  }
};

class DiffuseIter : public DownstreamVisitor
{
  Diffuse& d_data;
  geo::ScalarSimpleRaster d_inflow;
  geo::ScalarSimpleRaster d_outflow;
  size_t   d_edgeNr;

public:
  DiffuseIter(Diffuse& data):
    DownstreamVisitor(data.d_dem.rasterDim()),
    d_data(data),
    d_inflow(data.d_dem.rasterDim()),
    d_outflow(data.d_dem.rasterDim()),
    d_edgeNr(0)
  {}

  void initVertex(const Vertex& vC) {
    size_t v=linear(vC);
    d_data.addDem(v);
    d_inflow[v] =0;
    d_outflow[v]=0;
  }

  void finishVertex(const Vertex& vC) {
    if (pcr::isMV(d_data.d_dem[vC]))
      return;
    size_t v=linear(vC);
    if (!d_data.d_fixedHead[v]) {
      d_data.d_dem[v] += (d_inflow[v]-d_outflow[v])/d_data.d_area[v];
      d_data.checkDem(v);
    }
    d_data.d_totalOutflow[v]+=d_outflow[v];
  }


  void downstreamEdge(const Edge& e) {
   REAL4 drop= d_data.drop(e);
   if (drop < 0)
     return;
   DEVELOP_PRECOND(d_edgeNr < d_data.d_fixedEdgeFlowTerm.size());
   REAL4 edgeFlow = drop * d_data.d_fixedEdgeFlowTerm[d_edgeNr++];
   d_outflow[e.source()]+=edgeFlow;
   d_inflow[e.target()] +=edgeFlow;
  }

};

} // namespace mldd



//------------------------------------------------------------------------------
// DEFINITION OF STATIC DIFFUSE MEMBERS
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// DEFINITION OF DIFFUSE MEMBERS
//------------------------------------------------------------------------------

mldd::Diffuse::Diffuse(geo::ScalarSimpleRaster&  dem,
                       REAL4*       totalOutflow,
                       const REAL4* oldState,
                       const REAL4* area,
                       const REAL4* fixedHead,
                       const std::vector<const REAL4 *>&
                                    diffusionValueInArgOrder,
                       INT4         nrIterations,
                       double       cellSize):
  d_oldState (oldState),
  d_area     (area),
  d_fixedHead(fixedHead),
  d_diffusionValue(8),
  d_nrIterations(nrIterations),
  d_totalOutflow(totalOutflow),
  d_dem(dem),
  d_infinity(0),
  d_minInfinity(0)
{
  d_cellSize[0]=cellSize;
  d_cellSize[1]=cellSize*std::sqrt(2.0);
  arg2NBOrder(d_diffusionValue,diffusionValueInArgOrder);

  if (d_nrIterations <= 0) {
    // TODO operator name should be prepended in pcrme a runtimeerr
    //      also in checkAndExec!
    throw com::Exception("mldd::diffuse, nrIterations must be > 0");
  }
  pcr::setMV(d_totalOutflow, d_dem.rasterDim().nrCells());
}



mldd::Diffuse::~Diffuse()
{
 if (d_infinity || d_minInfinity) {
  std::cerr << std::endl;
  std::cerr << "Dem of diffuse has reached "<< d_infinity << " times +infinity " << std::endl;
  std::cerr << "Dem of diffuse has reached "<< d_minInfinity << " times -infinity " << std::endl;
 }
}

/* NOT IMPLEMENTED
//! Assignment operator.
mldd::Diffuse& mldd::Diffuse::operator=(const Diffuse& rhs)
{
  if (this != &rhs) {
  }
  return *this;
}

//! Copy constructor. NOT IMPLEMENTED.
mldd::Diffuse::Diffuse(const Diffuse& rhs)
{
}
*/

void mldd::Diffuse::run(const DagRaster& dr) {
  DiffuseInit init(*this,dr.nrEdges());
  dr.downstreamVisitor(init);
  for(int i=0; i < d_nrIterations; ++i) {
    DiffuseIter iter(*this);
    dr.downstreamVisitor(iter);
  }
}

void mldd::Diffuse::setResultMV(const size_t v)
{
    pcr::setMV(d_dem[v]);
    pcr::setMV(d_totalOutflow[v]);
}

void mldd::Diffuse::initSet(size_t v)
{
  d_totalOutflow[v]=0;
  if (com::oneIsMV(d_oldState[v],d_area[v],d_fixedHead[v]))
    setResultMV(v);
}

void mldd::Diffuse::initFlowTerm(const Edge& e)
{
  // c: index in [8] is NB code from e.source()->e.target()
  size_t c(geo::NB::code(e.source(),e.target()));
  // index in grid is s = e.source()
  size_t s,t;
  e.linear(s,t,d_dem.rasterDim());
  if (pcr::isMV(d_diffusionValue[c][s])) {

    setResultMV(s);
    setResultMV(t);
    d_fixedEdgeFlowTerm.push_back(0); // 0 don't care
  } else {
    d_fixedEdgeFlowTerm.push_back((d_diffusionValue[c][s]
          / d_nrIterations)
          / d_cellSize[geo::NB::diagonal(e.source(),e.target())]);

  }
}

void mldd::Diffuse::checkDem(size_t v)
{
   if (d_dem[v] == std::numeric_limits<REAL4>::infinity())
      d_infinity++; // throw std::range_error("dem has reached infinity");
   if (d_dem[v] == -std::numeric_limits<REAL4>::infinity())
      d_minInfinity++; // throw std::range_error("dem has reached infinity");
}

//! \todo rhs-expr is constant in iterations
void mldd::Diffuse::addDem(size_t v)
{
  if (!pcr::isMV(d_dem[v])) {
   d_dem[v]+=(d_oldState[v]/d_nrIterations)/d_area[v];
   checkDem(v);
  }
}

//! return < 0 if MV, >=0 otherwise
REAL4 mldd::Diffuse::drop(const Edge& e) const {
  REAL4 diff = com::subtract(d_dem[e.source()], d_dem[e.target()]);
  return pcr::isMV(diff) ? -1 : std::max(diff, 0.0F);
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



