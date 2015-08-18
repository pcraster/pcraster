#ifndef INCLUDED_CALC_WLDELFTHABITATTEST
#define INCLUDED_CALC_WLDELFTHABITATTEST



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

namespace calc {
  // WlDelftHabitat declarations.
}



namespace calc {



//! This class implements the unit tests for the WlDelftHabitat class.
class WlDelftHabitatTest
{

public:

                   WlDelftHabitatTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             testXmlParsing      ();

  void             testStatTable       ();

  void             testWLStatTests     ();

  static boost::unit_test::test_suite*     suite               ();

};

} // namespace calc

#endif
