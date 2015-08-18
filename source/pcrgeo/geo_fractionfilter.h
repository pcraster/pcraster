#ifndef INCLUDED_GEO_FRACTIONFILTER
#define INCLUDED_GEO_FRACTIONFILTER



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.
#ifndef INCLUDED_GEO_FILTER
#include "geo_filter.h"
#define INCLUDED_GEO_FILTER
#endif

// Module headers.



namespace geo {
  // FractionFilter declarations.
}



namespace geo {



//! Calculates the fraction of cells with a certain value within the kernel.
/*!
*/
template<class SourceValueType>
class FractionFilter: public Filter<SourceValueType, REAL8>
{

  friend class FractionFilterTest;

private:

  //! Value to count.
  SourceValueType       d_value;

  REAL8            result              (SimpleRaster<SourceValueType> const& source,
                                        size_t rowSrc,
                                        size_t colSrc,
                                        size_t rowFlt,
                                        size_t colFlt,
                                        size_t nrRows,
                                        size_t nrCols) const;

protected:

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   FractionFilter      (SimpleRaster<double>& weights,
                                        SourceValueType value);

  /* virtual */    ~FractionFilter     ();

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

template<class SourceValueType>
FractionFilter<SourceValueType>::FractionFilter(
         SimpleRaster<double>& weights,
         SourceValueType value)

  : Filter<SourceValueType, double>(weights),
    d_value(value)

{
}

template<class SourceValueType>
FractionFilter<SourceValueType>::~FractionFilter()
{
}

template<class SourceValueType>
REAL8 FractionFilter<SourceValueType>::result(
         SimpleRaster<SourceValueType> const& source,
         size_t rowSrc,
         size_t colSrc,
         size_t rowFlt,
         size_t colFlt,
         size_t nrRows,
         size_t nrCols) const
{
  REAL8 result = 0.0;
  REAL8 count = 0.0;

  for(size_t row = 0; row < nrRows; ++row) {
    for(size_t col = 0; col < nrCols; ++col) {
      if(!pcr::isMV(source.cell(rowSrc + row, colSrc + col))) {
        if(source.cell(rowSrc + row, colSrc + col) == d_value) {
          result += this->cell(rowFlt + row, colFlt + col);
        }

        count += this->cell(rowFlt + row, colFlt + col);
      }
    }
  }

  if(count > 0.0) {
    result /= count;
  }
  else {
    pcr::setMV(result);
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
