#ifndef INCLUDED_GEO_ALGORITHMTEST
#define INCLUDED_GEO_ALGORITHMTEST



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
  // Algorithm declarations.
}



namespace geo {



//! This class implements the unit tests for the Algorithm class.
class AlgorithmTest
{

private:

public:

                   AlgorithmTest       ();

  void             setUp               ();

  void             tearDown            ();

  void             testPointsInArea    ();

  void             testMaximum         ();

  static boost::unit_test::test_suite* suite();

};

} // namespace geo

#endif
