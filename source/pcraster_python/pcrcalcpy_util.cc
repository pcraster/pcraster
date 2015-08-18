#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_PCRCALCPY_UTIL
#include "pcrcalcpy_util.h"
#define INCLUDED_PCRCALCPY_UTIL
#endif

// Library headers.
#ifndef INCLUDED_SSTREAM
#include <sstream>
#define INCLUDED_SSTREAM
#endif

// PCRaster library headers.
#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif

#ifndef INCLUDED_GEO_RASTERSPACE
#include "geo_rasterspace.h"
#define INCLUDED_GEO_RASTERSPACE
#endif

// Module headers.



void pcrcalcpy::checkRasterSpace(
         const geo::RasterSpace& clone, const geo::RasterSpace& rasterSpace)
{
  if(clone != rasterSpace) {
    std::ostringstream stream;
    stream << "Spatial properties of raster differ from clone";
    throw com::Exception(stream.str());
  }
}
