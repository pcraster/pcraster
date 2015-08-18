#ifndef INCLUDED_CALC_AVERAGEMAPTEST
#define INCLUDED_CALC_AVERAGEMAPTEST



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
  // AverageMap declarations.
}



namespace calc {



//! This class implements the unit tests for the AverageMap class.
class AverageMapTest
{

private:

public:

                   AverageMapTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             test                ();

  static boost::unit_test::test_suite* suite();

};

} // namespace calc

#endif
