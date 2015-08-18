#ifndef INCLUDED_CALC_EXECUTORTEST
#define INCLUDED_CALC_EXECUTORTEST



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_CALC_ASTTESTFACTORY
#include "calc_asttestfactory.h"
#define INCLUDED_CALC_ASTTESTFACTORY
#endif



namespace boost {
  namespace unit_test {
    class test_suite;
  }
}

namespace calc {
  // Executor declarations.
}



namespace calc {



//! This class implements the unit tests for the Executor class.
class ExecutorTest: public ASTTestFactory
{

public:

                   ExecutorTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             testExpr            ();

  void             testLookup          ();

  void             testErrors          ();

  void             testRunTimeErrors   ();
  void             testDomainErrors    ();

  void             testAss             ();

  void             testModel           ();

  void             testRunDirectory    ();

  void             testDynamic         ();

  void             testUseDef          ();

  void             testUseDiskStorage  ();

  void             testExternalBindings();

  void             testSetStep         ();

  void             testBugs            ();

  void             testTimeStepHack    ();

  void             testLinkInLibrary   ();

  static boost::unit_test::test_suite*     suite               ();

};

} // namespace calc

#endif
