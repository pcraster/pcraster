#ifndef INCLUDED_TAB_CLASSCOUNTMAPTEST
#define INCLUDED_TAB_CLASSCOUNTMAPTEST



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

namespace tab {
  // ClassCountMap declarations.
}



namespace tab {



//! This class implements the unit tests for the ClassCountMap class.
class ClassCountMapTest
{

public:

                   ClassCountMapTest           ();

  void             testCountMap                ();
  void             testClassClassCountMap      ();

  static boost::unit_test::test_suite *    suite               ();

};

} // namespace tab

#endif
