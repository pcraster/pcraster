#ifndef INCLUDED_GEO_ADDFILTER
#define INCLUDED_GEO_ADDFILTER



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
                                        size_t nrCols) const;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   AddFilter           (const SimpleRaster<double>& weights);

  /* virtual */    ~AddFilter          ();

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
