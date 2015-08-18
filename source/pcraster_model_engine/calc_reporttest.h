#ifndef INCLUDED_CALC_REPORTTEST
#define INCLUDED_CALC_REPORTTEST



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
  // Report declarations.
}



namespace calc {



//! This class implements the unit tests for the Report class.
class ReportTest
{

public:

                   ReportTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             testReportDefault   ();

  static boost::unit_test::test_suite*     suite               ();

};

} // namespace calc

#endif
