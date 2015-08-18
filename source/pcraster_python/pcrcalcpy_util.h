#ifndef INCLUDED_PCRCALCPY_UTIL
#define INCLUDED_PCRCALCPY_UTIL



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.

// Module headers.



namespace geo {
  class RasterSpace;
}
namespace calc {
}
namespace pcrcalcpy {
  // Util declarations.
}



namespace pcrcalcpy {


void               checkRasterSpace    (
                        const geo::RasterSpace& clone,
                        const geo::RasterSpace& rasterSpace);



} // namespace pcrcalcpy

#endif
