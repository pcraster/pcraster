#ifndef INCLUDED_GEO_CSFRASTER
#define INCLUDED_GEO_CSFRASTER

#include "stddefx.h"
#include "geo_raster.h"



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

  /* virtual */    ~CSFRaster          () override;

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
