#ifndef INCLUDED_COM_INTERVALMAPTEST
#define INCLUDED_COM_INTERVALMAPTEST



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

namespace com {
  // IntervalMap declarations.
}



namespace com {



//! This class implements the unit tests for the IntervalMap class.
class IntervalMapTest
{

public:

                   IntervalMapTest     ();

  void             setUp               ();

  void             tearDown            ();

  void             testFind            ();

  void             testVisit           ();

  void             testOverlap         ();

  void             testMultiMap        ();

  void             testNoOverlap       ();

  static boost::unit_test::test_suite* suite();

};

} // namespace com

#endif
