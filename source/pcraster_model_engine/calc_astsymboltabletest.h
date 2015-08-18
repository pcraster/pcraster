#ifndef INCLUDED_CALC_ASTSYMBOLTABLETEST
#define INCLUDED_CALC_ASTSYMBOLTABLETEST



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
  // ASTSymbolTable declarations.
}



namespace calc {



//! This class implements the unit tests for the ASTSymbolTable class.
class ASTSymbolTableTest: public ASTTestFactory
{

public:

                   ASTSymbolTableTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             testResolve         ();

  static boost::unit_test::test_suite*     suite               ();

};

} // namespace calc

#endif
