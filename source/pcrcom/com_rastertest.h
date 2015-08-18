#ifndef INCLUDED_COM_RASTERTEST
#define INCLUDED_COM_RASTERTEST



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

namespace com {
  // Raster declarations.
}



namespace com {



//! This class implements the unit tests for the Raster class.
class RasterTest
{

public:

                   RasterTest          ();

  void             setUp               ();

  void             tearDown            ();

  void             testConstructorSingleValue  ();

  static boost::unit_test::test_suite* suite();

};

} // namespace com

#endif
