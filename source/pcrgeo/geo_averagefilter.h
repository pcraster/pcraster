#ifndef INCLUDED_GEO_AVERAGEFILTER
#define INCLUDED_GEO_AVERAGEFILTER

#include "stddefx.h"
#include "geo_filter.h"



namespace geo {
  // AverageFilter declarations.
}



namespace geo {



//! This filter calculates the average value of cells within the kernel.
/*!
*/
class AverageFilter: public Filter<double, double>
{

private:

  double           result              (const SimpleRaster<double>& source,
                                        size_t rowSrc,
                                        size_t colSrc,
                                        size_t rowFlt,
                                        size_t colFlt,
                                        size_t nrRows,
                                        size_t nrCols) const override;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   AverageFilter       (const SimpleRaster<double>& weights);

  /* virtual */    ~AverageFilter      () override;

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



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace geo

#endif
