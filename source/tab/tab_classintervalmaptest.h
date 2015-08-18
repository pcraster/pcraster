#ifndef INCLUDED_TAB_CLASSINTERVALMAPTEST
#define INCLUDED_TAB_CLASSINTERVALMAPTEST



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
  // classIntervalMap declarations.
}



namespace tab {



//! This class implements the unit tests for the classIntervalMap class.
class ClassIntervalMapTest
{

public:

                   ClassIntervalMapTest           ();

  void             test1               ();

  static boost::unit_test::test_suite *    suite               ();

};

} // namespace tab

#endif
