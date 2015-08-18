#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_GEO_ADDFILTER
#include "geo_addfilter.h"
#define INCLUDED_GEO_ADDFILTER
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
  This file contains the implementation of the AddFilter class.
*/



//------------------------------------------------------------------------------

/*
namespace geo {

class AddFilterPrivate
{
public:

  AddFilterPrivate()
  {
  }

  ~AddFilterPrivate()
  {
  }

};

} // namespace geo
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC ADDFILTER MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF ADDFILTER MEMBERS
//------------------------------------------------------------------------------

geo::AddFilter::AddFilter(const SimpleRaster<double>& weights)

  : Filter<double, double>(weights)

{
}



geo::AddFilter::~AddFilter()
{
}



//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
  \bug       Selection of weight factors incorrect!
*/
double geo::AddFilter::result(const SimpleRaster<double>& source,
         size_t rowSrc, size_t colSrc, size_t rowFlt, size_t colFlt,
         size_t nrRows, size_t nrCols) const
{
  double result = 0.0;

  for(CellLocVisitor loc(nrRows, nrCols); loc.valid(); ++loc) {
    if(!pcr::isMV(source.cell(rowSrc + loc.row(), colSrc + loc.col()))) {
      result += cell(rowFlt + loc.row(), colFlt + loc.col()) *
         source.cell(rowSrc + loc.row(), colSrc + loc.col());
    }
  }

  return result;
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------






//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



