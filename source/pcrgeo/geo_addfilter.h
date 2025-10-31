#ifndef INCLUDED_GEO_ADDFILTER
#define INCLUDED_GEO_ADDFILTER

#include "stddefx.h"
#include "geo_filter.h"
#include "geo_simpleraster.h"



namespace geo {
  // AddFilter declarations.
}



namespace geo {



//! This filter adds values of cells within the kernel.
/*!
*/
class AddFilter: public Filter<double, double>
{

private:

  //! Assignment operator. NOT IMPLEMENTED.
  AddFilter&       operator=           (const AddFilter&);

  //! Copy constructor. NOT IMPLEMENTED.
                   AddFilter           (const AddFilter&);

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

                   AddFilter           (const SimpleRaster<double>& weights);

  /* virtual */    ~AddFilter          () override;

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
