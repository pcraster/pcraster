#ifndef INCLUDED_CALC_FINDSYMBOLTEST
#define INCLUDED_CALC_FINDSYMBOLTEST



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
  // FindSymbol declarations.
}



namespace calc {



//! This class implements the unit tests for the FindSymbol class.
class FindSymbolTest
{

public:

                   FindSymbolTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             testOpName2op       ();

  static boost::unit_test::test_suite*     suite               ();

};

} // namespace calc

#endif
