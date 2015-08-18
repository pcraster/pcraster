#ifndef INCLUDED_CALC_POINTCODEBLOCKREPLACERTEST
#define INCLUDED_CALC_POINTCODEBLOCKREPLACERTEST



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
  // PointCodeBlockReplacer declarations.
}



namespace calc {



//! This class implements the unit tests for the PointCodeBlockReplacer class.
class PointCodeBlockReplacerTest
{

private:

public:

                   PointCodeBlockReplacerTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             test                ();

  static boost::unit_test::test_suite* suite();

};

} // namespace calc

#endif
