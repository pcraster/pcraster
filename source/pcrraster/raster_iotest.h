#ifndef INCLUDED_RASTER_IOTEST
#define INCLUDED_RASTER_IOTEST



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
  // IO declarations.
}



namespace raster {



//! This class implements the unit tests for the IO class.
class IOTest
{

private:

public:

                   IOTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             test                ();

  static boost::unit_test::test_suite* suite();

};

} // namespace raster

#endif
