#ifndef INCLUDED_GEO_FRACTIONFILTERTEST
#define INCLUDED_GEO_FRACTIONFILTERTEST



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
  // FractionFilter declarations.
}



namespace geo {



//! This class implements the unit tests for the FractionFilter class.
class FractionFilterTest
{

private:

public:

                   FractionFilterTest  ();

  void             setUp               ();

  void             tearDown            ();

  void             test                ();

  static boost::unit_test::test_suite* suite();

};

} // namespace geo

#endif
