#ifndef INCLUDED_CALC_CFGCREATORTEST
#define INCLUDED_CALC_CFGCREATORTEST



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

namespace com {
  // CFGCreator declarations.
}



namespace calc {



//! This class implements the unit tests for the CFGCreator class.
class CFGCreatorTest
{
public:

                   CFGCreatorTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             testExpr            ();
  void             testStatementList   ();
  void             testRewrites        ();

  void             testAssignment      ();
  void             testStatement       ();
  void             testCode            ();
  void             testModel           ();
  void             testBinding         ();
  void             testReportSection   ();
  void             testParseErrors     ();
  void             testModelCFGCreator     ();

  static boost::unit_test::test_suite*     suite               ();

};

} // namespace calc

#endif
