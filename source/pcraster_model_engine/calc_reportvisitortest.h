#ifndef INCLUDED_CALC_REPORTVISITORTEST
#define INCLUDED_CALC_REPORTVISITORTEST



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
  // ReportVisitor declarations.
}



namespace calc {



//! This class implements the unit tests for the ReportVisitor class.
class ReportVisitorTest
{

public:

                   ReportVisitorTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             testPos             ();

  static boost::unit_test::test_suite*     suite               ();

};

} // namespace calc

#endif
