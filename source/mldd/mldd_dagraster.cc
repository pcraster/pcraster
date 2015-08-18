#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_MLDD_DAGRASTER
#include "mldd_dagraster.h"
#define INCLUDED_MLDD_DAGRASTER
#endif

// Library headers.
#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
#endif

#ifndef INCLUDED_BOOST_GRAPH_TOPOLOGICAL_SORT
#include "boost/graph/topological_sort.hpp"
#define INCLUDED_BOOST_GRAPH_TOPOLOGICAL_SORT
#endif

// PCRaster library headers.
#ifndef INCLUDED_MISC
#include "misc.h"  // bitset functions
#define INCLUDED_MISC
#endif
#ifndef INCLUDED_GEO_NEIGHBOUR
#include "geo_neighbour.h"
#define INCLUDED_GEO_NEIGHBOUR
#endif
// Module headers.
#ifndef INCLUDED_MLDD_GRAPH
#include "mldd_graph.h"
#define INCLUDED_MLDD_GRAPH
#endif

#ifndef INCLUDED_MLDD_DOWNSTREAMVISITOR
#include "mldd_downstreamvisitor.h"
#define INCLUDED_MLDD_DOWNSTREAMVISITOR
#endif

/*!
  \file
  This file contains the implementation of the DagRaster class.
*/



//------------------------------------------------------------------------------

/*
namespace mldd {

class DagRasterPrivate
{
public:

  DagRasterPrivate()
  {
  }

  ~DagRasterPrivate()
  {
  }

};

} // namespace mldd
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC DAGRASTER MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF DAGRASTER MEMBERS
//------------------------------------------------------------------------------

mldd::DagRaster::DagRaster(const geo::RasterDim& rd):
  d_0(0),
  d_rd(rd),
  d_outflowNB(rd,d_0),
  d_inflowNB (rd,d_0)
{
}



mldd::DagRaster::~DagRaster()
{
}

/*
//! Assignment operator.
mldd::DagRaster& mldd::DagRaster::operator=(const DagRaster& rhs)
{
  if (this != &rhs) {
  }
  return *this;
}

//! Copy constructor. DEFAULT
mldd::DagRaster::DagRaster(const DagRaster& rhs)
{
}
*/

void  mldd::DagRaster::addInflowNB(const geo::CellLoc& c, geo::NB::Code nb)
{
  d_inflowNB[c] |= 1<<nb;
}
void  mldd::DagRaster::addOutflowNB(const geo::CellLoc& c, geo::NB::Code nb)
{
  d_outflowNB[c] |= 1<<nb;
}

void  mldd::DagRaster::addFlowNB(size_t row,size_t col, geo::LDD::Code lc)
{
  addFlowNB(geo::CellLoc(row,col),lc);
}

/*!
 * \todo
 *   document that pits can not be added?
 */
void  mldd::DagRaster::addFlowNB(
    const geo::CellLoc& outflowC,
    geo::LDD::Code lc)
{
  PRECOND(lc != 5);
  geo::NB::Code oc= geo::LDD::toNB(lc);
  // addOutflowNB(outflowC,oc); NO only add if flows to inside map
  geo::CellLoc inflowC = geo::NB::target(outflowC,oc);
  if (d_rd.contains(inflowC)) {
    addFlow(outflowC,inflowC);
    // addOutflowNB(outflowC,oc);
    // addInflowNB(inflowC,geo::NB::reverse(oc));
  }
}

void  mldd::DagRaster::addFlow(
    const geo::CellLoc& from,
    const geo::CellLoc& to)
{
  geo::NB::Code oc= geo::NB::code(from,to);
  addOutflowNB(from,oc);
  addInflowNB(to,geo::NB::reverse(oc));
}


size_t mldd::DagRaster::nrInflowNB(const geo::CellLoc& c) const
{
  return NRBITSET_TYPE(d_inflowNB[c],EightBits);
}

size_t mldd::DagRaster::nrOutflowNB(const geo::CellLoc& c) const
{
  return NRBITSET_TYPE(d_outflowNB[c],EightBits);
}

bool   mldd::DagRaster::isPit(const size_t c) const
{
  return d_inflowNB[c] > 0 && d_outflowNB[c]==0;
}

bool   mldd::DagRaster::hasInflowNB(const geo::CellLoc& c) const
{
  return d_inflowNB[c];
}

bool   mldd::DagRaster::hasOutflowNB(const geo::CellLoc& c) const
{
  return d_outflowNB[c];
}


//! clear all data
void mldd::DagRaster::clear()
{
  d_outflowNB.fill(0);
  d_inflowNB.fill(0);
  d_rto.clear();
}


const geo::RasterDim& mldd::DagRaster::rasterDim() const
{
  return d_rd;
}

size_t mldd::DagRaster::nrVertices() const
{
  /* NO: COUNT BOTH, reason to integrate in and outflow in one struct
  return
  std::count_if(d_outflowNB.begin(), d_outflowNB.end(),
      std::bind2nd(std::not_equal_to<int>(),0));
   */
  size_t count=0;
  for(geo::LinearLoc i=0; i < d_outflowNB.nrCells(); i++)
    count += isVertex(i);
  return count;
}

size_t mldd::DagRaster::nrEdges() const
{
  size_t count=0;
  for(geo::LinearLoc i=0; i < d_outflowNB.nrCells(); i++)
    count += NRBITSET_TYPE(d_outflowNB[i],EightBits);
  return count;
}

bool mldd::DagRaster::hasOutflowDir(
    geo::LinearLoc c,
    geo::NB::Code dir) const
{
  return d_outflowNB[c] & (1<<dir);
}

bool mldd::DagRaster::isVertex(geo::LinearLoc l) const
{
  return d_outflowNB[l] || d_inflowNB[l];
}

mldd::Vertex mldd::DagRaster::nextVertex(const Vertex& c) const
{
  geo::LinearLoc l(d_rd.convert(c));
  do {
    l++;
  } while (l < d_rd.nrCells() && !isVertex(l));
  return d_rd.convert(l);
}

//! return begin for VertexIterator
mldd::Vertex mldd::DagRaster::beginVertex() const
{
  // first cell
  geo::CellLoc b=d_rd.convert(0);
  if (isVertex(0))
    return b;
  // if first cell is not a vertex, proceed from first
  return nextVertex(b);
}

//! return end for VertexIterator
mldd::Vertex mldd::DagRaster::endVertex() const
{
  return d_rd.convert(d_rd.nrCells());
}

//! return iterator for all out edges of c
mldd::OutEdgeIterator mldd::DagRaster::beginOutEdge(const Vertex& c) const
{
  return OutEdgeIterator(c,d_outflowNB[c],0);
}

//! return end-iterator for all out edges of c (if any() is not usable)
mldd::OutEdgeIterator mldd::DagRaster::endOutEdge(const   Vertex& c) const
{
  return OutEdgeIterator(c);
}


void mldd::DagRaster::downstreamVisitor(
    DownstreamVisitor& dv) const
{
  for(size_t i=0; i<d_rto.size(); ++i)
    dv.initVertex(d_rto[i]);
  for(RTO::const_reverse_iterator i=d_rto.rbegin(); i!=d_rto.rend();++i) {
    for(OutEdgeIterator e=beginOutEdge(*i);e.any();++e)
       dv.downstreamEdge(*e);
    dv.finishVertex(*i);
  }
}

void mldd::DagRaster::remove(
    const DagRaster& rm)
{
  PRECOND(rasterDim()==rm.rasterDim());
  for(size_t i=0; i < d_outflowNB.nrCells(); ++i) {
    d_inflowNB[i]  ^= rm.d_inflowNB[i];
    d_outflowNB[i] ^= rm.d_outflowNB[i];
  }
  updateOrder();
}

//! recompute the topological order
void mldd::DagRaster::updateOrder()
{
  std::vector<boost::default_color_type> raster(d_rd.nrCells());
  typedef DagRasterPropertyMap<boost::default_color_type> Color;

  Color colorMap(d_rd,raster);

  d_rto.clear();

  try {
  boost::topological_sort(*this,
      std::back_inserter(d_rto),
      boost::color_map(colorMap));
  } catch (const boost::not_a_dag&) {
     throw NotADag();
  }
}

#include <boost/graph/graph_concepts.hpp>
#include <boost/property_map/property_map.hpp>

void mldd::DagRaster::conceptCheck() const
{

#ifdef BOOST_NO_STD_ITERATOR_TRAITS
#error This examples requires a compiler that provides a working std::iterator_traits
#endif

  using namespace boost;
  using namespace mldd;

  typedef mldd::DagRasterPropertyMap<int>  DrpInt;
  function_requires < ReadWritePropertyMapConcept < DrpInt, mldd::Vertex > >();
  function_requires < GraphConcept              < DagRaster > >();
  function_requires < VertexListGraphConcept    < DagRaster > >();
  function_requires < IncidenceGraphConcept     < DagRaster > >();

// Not needed:
//  function_requires < BidirectionalGraphConcept < DagRaster > >();
//  function_requires < VertexMutableGraphConcept < DagRaster > >();
//  function_requires < EdgeMutableGraphConcept   < DagRaster > >();

}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

void  mldd::Edge::linear(
    size_t& s,
    size_t& t,
    const geo::RasterDim& rd) const
{
  s=rd.convert(source());
  t=rd.convert(target());
}


