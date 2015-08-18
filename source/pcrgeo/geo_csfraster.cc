#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_GEO_CSFRASTER
#include "geo_csfraster.h"
#define INCLUDED_GEO_CSFRASTER
#endif

// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_GEO_SAMECELLREPR
#include "geo_samecellrepr.h"
#define INCLUDED_GEO_SAMECELLREPR
#endif

#ifndef INCLUDED_GEO_UTIL
#include "geo_util.h"
#define INCLUDED_GEO_UTIL
#endif

#ifndef INCLUDED_GEO_CSFMAP
#include "geo_csfmap.h"
#define INCLUDED_GEO_CSFMAP
#endif


/*!
  \file
  This file contains the implementation of the CSFRaster class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC CSFRASTER MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF CSFRASTER MEMBERS
//------------------------------------------------------------------------------

//! create and read data from map
/*!
   Note that \a map is modified for 'useAs' cell type
 */
template<class T>
geo::CSFRaster<T>::CSFRaster(CSFMap& map)

  : Raster<T>(map.rasterSpace()) // CSFMap has RasterSpace conversion

{
  // Side effect.
  map.useAs(SameCellRepr<T>::cr);

  // allocate(); already done in Raster constructor.

  // Side effect.
  map.getCells(this->cells());
}



//! dtor
template<class T>
geo::CSFRaster<T>::~CSFRaster()
{
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



