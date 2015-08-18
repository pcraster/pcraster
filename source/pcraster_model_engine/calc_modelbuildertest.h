#ifndef INCLUDED_CALC_MODELBUILDERTEST
#define INCLUDED_CALC_MODELBUILDERTEST



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
  // ModelBuilder declarations.
}



namespace calc {



//! This class implements the unit tests for the ModelBuilder class.
class ModelBuilderTest
{

public:

                   ModelBuilderTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             testSingleStatement ();

  void             testASTExpr       ();

  void             testMultipleStatements();

  void             testMultipleStatementsWithError();

  void             testMultipleStatementsWithBinding();

  void             testSetValuescale();

  void             testAddLookupTable();

  void             testExternalBindings();

  static boost::unit_test::test_suite*     suite               ();

};

} // namespace calc

#endif
