#ifndef INCLUDED_CALC_RUNDIRECTORYTEST
#define INCLUDED_CALC_RUNDIRECTORYTEST



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
  // RunDirectory declarations.
}



namespace calc {



//! This class implements the unit tests for the RunDirectory class.
class RunDirectoryTest
{

public:

                   RunDirectoryTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             testDefault         ();

  void             testRunDir          ();

  void             testSearchPath      ();

  void             testOutputFilePath  ();

  static boost::unit_test::test_suite*     suite               ();

};

} // namespace calc

#endif
