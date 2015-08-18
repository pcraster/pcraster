#ifndef INCLUDED_GEO_AVERAGEFILTER
#define INCLUDED_GEO_AVERAGEFILTER



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
                                        size_t nrCols) const;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   AverageFilter       (const SimpleRaster<double>& weights);

  /* virtual */    ~AverageFilter      ();

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
