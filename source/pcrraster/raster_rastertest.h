#ifndef INCLUDED_RASTER_RASTERTEST
#define INCLUDED_RASTER_RASTERTEST



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.

// Module headers.



namespace boost {
  namespace unit_test {
    class test_suite;
  }
}

namespace raster {
  // Raster declarations.
}



namespace raster {



//! This class implements the unit tests for the Raster class.
class RasterTest
{

private:

public:

                   RasterTest          ();

  void             setUp               ();

  void             tearDown            ();

  void             test                ();

  void             testSetMV           ();

  static boost::unit_test::test_suite* suite();

};

} // namespace raster

#endif
