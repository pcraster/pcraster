#ifndef INCLUDED_GEO_AVERAGEFILTERTEST
#define INCLUDED_GEO_AVERAGEFILTERTEST



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
  // AverageFilter declarations.
}



namespace geo {



//! This class implements the unit tests for the AverageFilter class.
class AverageFilterTest
{

public:

                   AverageFilterTest   ();

  void             setUp               ();

  void             tearDown            ();

  void             test                ();

  static boost::unit_test::test_suite* suite();

};

} // namespace geo

#endif
