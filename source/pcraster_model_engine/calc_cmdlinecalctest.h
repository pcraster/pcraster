#ifndef INCLUDED_CALC_CMDLINECALCTEST
#define INCLUDED_CALC_CMDLINECALCTEST



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
  // CmdLineCalc declarations.
}



namespace calc {



//! This class implements the unit tests for the CmdLineCalc class.
class CmdLineCalcTest
{

private:

public:

                   CmdLineCalcTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             testScriptFile      ();
  void             testModelAsArgs     ();

  static boost::unit_test::test_suite* suite();

};

} // namespace calc

#endif
