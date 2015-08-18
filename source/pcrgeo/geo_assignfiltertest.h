#ifndef INCLUDED_GEO_ASSIGNFILTERTEST
#define INCLUDED_GEO_ASSIGNFILTERTEST



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
  // AssignFilter declarations.
}



namespace geo {



//! This class implements the unit tests for the AssignFilter class.
class AssignFilterTest
{

public:

                   AssignFilterTest    ();

  void             setUp               ();

  void             tearDown            ();

  void             test                ();

  static boost::unit_test::test_suite* suite();

};

} // namespace geo

#endif
