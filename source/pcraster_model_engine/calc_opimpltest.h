#ifndef INCLUDED_CALC_OPIMPLTEST
#define INCLUDED_CALC_OPIMPLTEST



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
  // OpImpl declarations.
}



namespace calc {



//! This class implements the unit tests for the OpImpl class.
class OpImplTest
{

public:

                   OpImplTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             testSameBin         ();

  void             testCover           ();

  void             testSameUn          ();

  void             testTrig            ();

  void             testDiffUn          ();

  void             testGen             ();

  void             testGlobal          ();

  void             testMRF             ();

  void             testConversion      ();

  void             testLookup          ();

  void             testTimeinputTssOp  ();


  void             testCompare         ();

  void             testIfThen         ();

  void             testIfThenElse     ();

  void             testDomainError     ();

  static boost::unit_test::test_suite*     suite               ();

};

} // namespace calc

#endif
