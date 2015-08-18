#ifndef INCLUDED_CALC_BINDINGTABLETEST
#define INCLUDED_CALC_BINDINGTABLETEST



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
  // BindingTable declarations.
}



namespace calc {



//! This class implements the unit tests for the BindingTable class.
class BindingTableTest: public ASTTestFactory
{

public:

                   BindingTableTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             testApplyErrors     ();

  void             testApply           ();

  static boost::unit_test::test_suite*     suite               ();

};

} // namespace calc

#endif
