#ifndef INCLUDED_GEO_FILECREATETESTERTEST
#define INCLUDED_GEO_FILECREATETESTERTEST



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
  // FileCreateTester declarations.
}



namespace geo {



//! This class implements the unit tests for the FileCreateTester class.
class FileCreateTesterTest
{

public:

                   FileCreateTesterTest();

  void             setUp               ();

  void             tearDown            ();

  void             testEqualToCsf      ();
  void             testEqualToTss      ();

  static boost::unit_test::test_suite* suite();

};

} // namespace geo

#endif
