#ifndef INCLUDED_GEO_POINTVALUETEST
#define INCLUDED_GEO_POINTVALUETEST



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
  // PointValue declarations.
}



namespace geo {



//! This class implements the unit tests for the PointValue class.
class PointValueTest
{

private:

public:

                   PointValueTest      ();

  void             setUp               ();

  void             tearDown            ();

  void             test                ();

  static boost::unit_test::test_suite* suite();

};

} // namespace geo

#endif
