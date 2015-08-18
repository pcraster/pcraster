#ifndef INCLUDED_GEO_CSFRASTER
#define INCLUDED_GEO_CSFRASTER



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_GEO_RASTER
#include "geo_raster.h"
#define INCLUDED_GEO_RASTER
#endif



namespace geo {
  // CSFRaster declarations.
  class CSFMap;
}



namespace geo {



//! A Raster initialized from a CSFMap
template<class T>
class CSFRaster: public Raster<T>
{

private:

  //! Assignment operator. NOT IMPLEMENTED.
  CSFRaster&       operator=           (const CSFRaster&);

  //! Copy constructor. NOT IMPLEMENTED.
                   CSFRaster           (const CSFRaster&);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   CSFRaster           (CSFMap& map);

  /* virtual */    ~CSFRaster          ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

};



//------------------------------------------------------------------------------
// INLINE FUNCIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace geo

#endif
