#ifndef INCLUDED_CALC_USEDEFANALYZERTEST
#define INCLUDED_CALC_USEDEFANALYZERTEST



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
  // UseDefAnalyzer declarations.
}



namespace calc {



//! This class implements the unit tests for the UseDefAnalyzer class.
class UseDefAnalyzerTest
{

private:

public:

                   UseDefAnalyzerTest  ();

  void             setUp               ();

  void             tearDown            ();

  void             testLinear          ();

  void             testBugs            ();

  void             nestedLoops         ();

  void             testIOTypes         ();

  void             testLoops           ();

  static boost::unit_test::test_suite* suite();

};

} // namespace calc

#endif
