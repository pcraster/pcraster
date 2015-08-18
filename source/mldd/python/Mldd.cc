#ifndef INCLUDED_MLDD
#include "Mldd.h"
#define INCLUDED_MLDD
#endif

// External headers.
#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
#endif

#ifndef INCLUDED_BOOST_FOREACH
#include <boost/foreach.hpp>
#define INCLUDED_BOOST_FOREACH
#endif

// Project headers.
#ifndef INCLUDED_CALC_SPATIAL
#include "calc_spatial.h"
#define INCLUDED_CALC_SPATIAL
#endif

// Module headers.



/*!
  \file
  This file contains the implementation of the Mldd class.
*/

// namespace {
// 
// } // Anonymous namespace



namespace mldd {
namespace python {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC MLDD MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF MLDD MEMBERS
//------------------------------------------------------------------------------

Mldd::Mldd(
         geo::RasterSpace const& space)

  : _mldd(space)

{
}



Mldd::~Mldd()
{
}



void Mldd::setDem(
         calc::Field const* dem)
{
  // TODO Check value type.
  _mldd.setDem(dem->src_f());
}



void Mldd::setStream(
         calc::Field const* lddEast,
         calc::Field const* lddNorthEast,
         calc::Field const* lddNorth,
         calc::Field const* lddNorthWest,
         calc::Field const* lddSouthEast,
         calc::Field const* lddSouth,
         calc::Field const* lddSouthWest,
         calc::Field const* lddWest)
{
  // TODO Check value type.
  std::vector<UINT1 const*> ldd;
  ldd.push_back(lddEast->src_1());
  ldd.push_back(lddNorthEast->src_1());
  ldd.push_back(lddNorth->src_1());
  ldd.push_back(lddNorthWest->src_1());
  ldd.push_back(lddSouthEast->src_1());
  ldd.push_back(lddSouth->src_1());
  ldd.push_back(lddSouthWest->src_1());
  ldd.push_back(lddWest->src_1());
  _mldd.setStream(ldd);
}



void Mldd::addStream(
         calc::Field const* ldd)
{
  // TODO Check value type.
  _mldd.addStream(ldd->src_1());
}



void Mldd::removeStream(
         calc::Field const* mark1,
         calc::Field const* mark2,
         calc::Field const* mark3,
         calc::Field const* mark4,
         calc::Field const* mark5,
         calc::Field const* mark6,
         calc::Field const* mark7,
         calc::Field const* mark8)
{
  // TODO Check value type.
  std::vector<UINT1 const*> marks;

  marks.push_back(mark1->src_1());
  marks.push_back(mark2->src_1());
  marks.push_back(mark3->src_1());
  marks.push_back(mark4->src_1());
  marks.push_back(mark5->src_1());
  marks.push_back(mark6->src_1());
  marks.push_back(mark7->src_1());
  marks.push_back(mark8->src_1());

  _mldd.removeStream(marks);
}



boost::shared_ptr<calc::Field> Mldd::diffuse(
         calc::Field const* oldState,
         calc::Field const* area,
         calc::Field const* fixedHead,
         calc::Field const* value1,
         calc::Field const* value2,
         calc::Field const* value3,
         calc::Field const* value4,
         calc::Field const* value5,
         calc::Field const* value6,
         calc::Field const* value7,
         calc::Field const* value8,
         INT4 nrIterations)
{
  // TODO Check value type.
  boost::shared_ptr<calc::Spatial> totalOutFlow(new calc::Spatial(VS_S,
         calc::CRI_f, _mldd.space().nrCells()));

  std::vector<REAL4 const*> values;
  values.push_back(value1->src_f());
  values.push_back(value2->src_f());
  values.push_back(value3->src_f());
  values.push_back(value4->src_f());
  values.push_back(value5->src_f());
  values.push_back(value6->src_f());
  values.push_back(value7->src_f());
  values.push_back(value8->src_f());

  _mldd.diffuse(totalOutFlow->dest_f(), oldState->src_f(), area->src_f(),
         fixedHead->src_f(), values, nrIterations);

  return totalOutFlow;
}



boost::python::tuple Mldd::getStream() const
{
  std::vector<calc::Spatial*> spatials(8);

  // Overwrite pointer addresses themselves.
  BOOST_FOREACH(calc::Spatial*& spatial, spatials) {
    spatial = new calc::Spatial(VS_L, calc::CRI_1, _mldd.space().nrCells());
  }

  std::vector<UINT1*> arrays(spatials.size());

  for(size_t i = 0; i < spatials.size(); ++i) {
    arrays[i] = spatials[i]->dest_1();
  }

  _mldd.getStream(arrays);

  return boost::python::make_tuple(
         boost::shared_ptr<calc::Field>(spatials[0]),
         boost::shared_ptr<calc::Field>(spatials[1]),
         boost::shared_ptr<calc::Field>(spatials[2]),
         boost::shared_ptr<calc::Field>(spatials[3]),
         boost::shared_ptr<calc::Field>(spatials[4]),
         boost::shared_ptr<calc::Field>(spatials[5]),
         boost::shared_ptr<calc::Field>(spatials[6]),
         boost::shared_ptr<calc::Field>(spatials[7]));
}



boost::python::tuple Mldd::getWeight() const
{
  std::vector<calc::Spatial*> spatials(8);

  // Overwrite pointer addresses themselves.
  BOOST_FOREACH(calc::Spatial*& spatial, spatials) {
    spatial = new calc::Spatial(VS_S, calc::CRI_f, _mldd.space().nrCells());
  }

  std::vector<REAL4*> arrays(spatials.size());

  for(size_t i = 0; i < spatials.size(); ++i) {
    arrays[i] = spatials[i]->dest_f();
  }

  _mldd.getWeight(arrays);

  return boost::python::make_tuple(
         boost::shared_ptr<calc::Field>(spatials[0]),
         boost::shared_ptr<calc::Field>(spatials[1]),
         boost::shared_ptr<calc::Field>(spatials[2]),
         boost::shared_ptr<calc::Field>(spatials[3]),
         boost::shared_ptr<calc::Field>(spatials[4]),
         boost::shared_ptr<calc::Field>(spatials[5]),
         boost::shared_ptr<calc::Field>(spatials[6]),
         boost::shared_ptr<calc::Field>(spatials[7]));
}



boost::shared_ptr<calc::Field> Mldd::getDem() const
{
  boost::shared_ptr<calc::Spatial> result(new calc::Spatial(VS_S, calc::CRI_f,
         _mldd.space().nrCells()));
  _mldd.getDem(result->dest_f());

  return result;
}



boost::shared_ptr<calc::Field> Mldd::upstream(
         calc::Field const* material)
{
  // TODO Check value type.
  boost::shared_ptr<calc::Spatial>result(new calc::Spatial(VS_S, calc::CRI_f,
         _mldd.space().nrCells()));
  _mldd.upstream(result->dest_f(), material->src_f());

  return result;
}



boost::shared_ptr<calc::Field> Mldd::accuflux(
         calc::Field const* material)
{
  // TODO Check value type.
  boost::shared_ptr<calc::Spatial> result(new calc::Spatial(VS_S, calc::CRI_f,
         _mldd.space().nrCells()));
  _mldd.accuflux(result->dest_f(), material->src_f());

  return result;
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace python
} // namespace mldd

