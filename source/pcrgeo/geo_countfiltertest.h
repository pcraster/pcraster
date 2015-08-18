#ifndef INCLUDED_GEO_COUNTFILTERTEST
#define INCLUDED_GEO_COUNTFILTERTEST



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
  // CountFilter declarations.
}



namespace geo {



//! This class implements the unit tests for the CountFilter class.
class CountFilterTest
{

public:

                   CountFilterTest     ();

  void             setUp               ();

  void             tearDown            ();

  void             test                ();

  static boost::unit_test::test_suite* suite();

};

} // namespace geo

#endif
