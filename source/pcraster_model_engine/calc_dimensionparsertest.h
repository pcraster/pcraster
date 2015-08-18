#ifndef INCLUDED_CALC_DIMENSIONPARSERTEST
#define INCLUDED_CALC_DIMENSIONPARSERTEST



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
  // DimensionParser declarations.
}



namespace calc {



//! This class implements the unit tests for the DimensionParser class.
class DimensionParserTest
{

private:

public:

                   DimensionParserTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             test                ();

  void             testError           ();

  static boost::unit_test::test_suite* suite();

};

} // namespace calc

#endif
