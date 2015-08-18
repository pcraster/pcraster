#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_GEO_AVERAGEFILTER
#include "geo_averagefilter.h"
#define INCLUDED_GEO_AVERAGEFILTER
#endif

// Library headers.
#ifndef INCLUDED_COM_CSFCELL
#include "com_csfcell.h"
#define INCLUDED_COM_CSFCELL
#endif

// PCRaster library headers.
#ifndef INCLUDED_GEO_CELLLOCVISITOR
#include "geo_celllocvisitor.h"
#define INCLUDED_GEO_CELLLOCVISITOR
#endif

// Module headers.



/*!
  \file
  This file contains the implementation of the AverageFilter class.
*/



//------------------------------------------------------------------------------

/*
namespace geo {

class AverageFilterPrivate
{
public:

  AverageFilterPrivate()
  {
  }

  ~AverageFilterPrivate()
  {
  }

};

} // namespace geo
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC AVERAGEFILTER MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF AVERAGEFILTER MEMBERS
//------------------------------------------------------------------------------

geo::AverageFilter::AverageFilter(const SimpleRaster<double>& weights)

  : Filter<double, double>(weights)

{
}



geo::AverageFilter::~AverageFilter()
{
}



double geo::AverageFilter::result(const SimpleRaster<double>& source,
         size_t rowSrc, size_t colSrc, size_t rowFlt, size_t colFlt,
         size_t nrRows, size_t nrCols) const
{
  double result = 0.0;
  double count = 0.0;

  for(CellLocVisitor loc(nrRows, nrCols); loc.valid(); ++loc) {
    if(!pcr::isMV(source.cell(rowSrc + loc.row(), colSrc + loc.col()))) {
      result += cell(rowFlt + loc.row(), colFlt + loc.col()) *
         source.cell(rowSrc + loc.row(), colSrc + loc.col());
      count += cell(rowFlt + loc.row(), colFlt + loc.col());
    }
  }

  if(count != 0.0) {
    result /= count;
  }
  else {
    pcr::setMV(result);
  }

  return result;
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



