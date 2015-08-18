#ifndef INCLUDED_GEO_COUNTFILTER
#define INCLUDED_GEO_COUNTFILTER



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_GEO_CELLLOCVISITOR
#include "geo_celllocvisitor.h"
#define INCLUDED_GEO_CELLLOCVISITOR
#endif

#ifndef INCLUDED_GEO_FILTER
#include "geo_filter.h"
#define INCLUDED_GEO_FILTER
#endif



namespace geo {
  // CountFilter declarations.
}



namespace geo {



//! short_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
/*!
  longer_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
*/
template<class SrcType, class DstType>
class CountFilter: public Filter<SrcType, DstType>
{

private:

  //! Value to count.
  SrcType          d_value;

  //! Assignment operator. NOT IMPLEMENTED.
  CountFilter&     operator=           (const CountFilter&);

  //! Copy constructor. NOT IMPLEMENTED.
                   CountFilter         (const CountFilter&);

  DstType          result              (const SimpleRaster<SrcType>& source,
                                        size_t rowSrc,
                                        size_t colSrc,
                                        size_t rowFlt,
                                        size_t colFlt,
                                        size_t nrRows,
                                        size_t nrCols) const;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   CountFilter         (const SimpleRaster<double>& weights,
                                        SrcType value);

  /* virtual */    ~CountFilter        ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

};



//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------

template<class SrcType, class DstType>
CountFilter<SrcType, DstType>::CountFilter(const SimpleRaster<double>& weights,
         SrcType value)
  : Filter<SrcType, DstType>(weights),
    d_value(value)
{
}

template<class SrcType, class DstType>
CountFilter<SrcType, DstType>::~CountFilter()
{
}

//! Counts the number of occurences of a value within the filter.
/*!
  The value to look for is the value given to the constructor. If the value
  is found in a cell within the filter the corresponding weighting factor is
  added to the resulting value of the filter.
*/
template<class SrcType, class DstType>
DstType CountFilter<SrcType, DstType>::result(
         const SimpleRaster<SrcType>& source,
         size_t rowSrc, size_t colSrc, size_t rowFlt, size_t colFlt,
         size_t nrRows, size_t nrCols) const
{
  DstType result(0);

  for(CellLocVisitor loc(nrRows, nrCols); loc.valid(); ++loc) {

    if(!pcr::isMV(source.cell(rowSrc + loc.row(), colSrc + loc.col())) &&
       source.cell(rowSrc + loc.row(), colSrc + loc.col()) == d_value) {

      // Found the value, add weighting factor.
      result += static_cast<DstType>(
         this->cell(rowFlt + loc.row(), colFlt + loc.col()));
    }
  }

  return result;
}



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace geo

#endif
