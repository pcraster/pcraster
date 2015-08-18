#ifndef INCLUDED_GEO_RASTERFILECONVERTERTEST
#define INCLUDED_GEO_RASTERFILECONVERTERTEST



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

namespace geo {
  // RasterFileConverter declarations.
}



namespace geo {



//! This class implements the unit tests for the RasterFileConverter class.
class RasterFileConverterTest
{

public:

                   RasterFileConverterTest();

  void             setUp               ();

  void             tearDown            ();

  void             testBil2Ascii       ();

  static boost::unit_test::test_suite* suite();

};

} // namespace geo

#endif
