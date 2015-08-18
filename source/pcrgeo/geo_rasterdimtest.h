#ifndef INCLUDED_GEO_RASTERDIMTEST
#define INCLUDED_GEO_RASTERDIMTEST



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
  // RasterDim declarations.
}



namespace geo {



//! This class implements the unit tests for the RasterDim class.
class RasterDimTest
{

public:

                   RasterDimTest       ();

  void             setUp               ();

  void             tearDown            ();

  void             testTarget          ();

  static boost::unit_test::test_suite* suite();

};

} // namespace geo

#endif
