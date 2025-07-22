#include "stddefx.h"
#include "geo_assignfilter.h"


/*!
  \file
  This file contains the implementation of the AssignFilter class.
*/



//------------------------------------------------------------------------------

/*
namespace geo {

class AssignFilterPrivate
{
public:

  AssignFilterPrivate()
  {
  }

  ~AssignFilterPrivate()
  {
  }

};

} // namespace geo
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC ASSIGNFILTER MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF ASSIGNFILTER MEMBERS
//------------------------------------------------------------------------------

geo::AssignFilter::AssignFilter(const SimpleRaster<double>& weights)

  : Filter<int, int>(weights)

{
}



geo::AssignFilter::~AssignFilter()
{
}



int geo::AssignFilter::calcUL(const SimpleRaster<int>& source,
                   size_t row, size_t col) const
{
  return source.cell(row, col);
}



int geo::AssignFilter::calcUR(const SimpleRaster<int>& source,
                   size_t row, size_t col) const
{
  return source.cell(row, col);
}



int geo::AssignFilter::calcLR(const SimpleRaster<int>& source,
                   size_t row, size_t col) const
{
  return source.cell(row, col);
}



int geo::AssignFilter::calcLL(const SimpleRaster<int>& source,
                   size_t row, size_t col) const
{
  return source.cell(row, col);
}



int geo::AssignFilter::calcTop(const SimpleRaster<int>& source,
                   size_t row, size_t col) const
{
  return source.cell(row, col);
}



int geo::AssignFilter::calcBottom(const SimpleRaster<int>& source,
                   size_t row, size_t col) const
{
  return source.cell(row, col);
}



int geo::AssignFilter::calcLeft(const SimpleRaster<int>& source,
                   size_t row, size_t col) const
{
  return source.cell(row, col);
}



int geo::AssignFilter::calcRight(const SimpleRaster<int>& source,
                   size_t row, size_t col) const
{
  return source.cell(row, col);
}



int geo::AssignFilter::calcInterior(const SimpleRaster<int>& source,
                   size_t row, size_t col) const
{
  return source.cell(row, col);
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



