#ifndef INCLUDED_DISCR_RASTERTEST
#define INCLUDED_DISCR_RASTERTEST



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

namespace discr {
  // Raster declarations.
}



namespace discr {



//! This class implements the unit tests for the Raster class.
class RasterTest
{

private:

public:

                   RasterTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             testConstructor     ();

  void             testEquals          ();

  static boost::unit_test::test_suite* suite();

};

} // namespace discr

#endif
