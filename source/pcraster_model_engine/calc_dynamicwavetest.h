#ifndef INCLUDED_CALC_DYNAMICWAVETEST
#define INCLUDED_CALC_DYNAMICWAVETEST



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
  // DynamicWave declarations.
}



namespace calc {



//! This class implements the unit tests for the DynamicWave class.
class DynamicWaveTest: public ASTTestFactory
{

public:

                   DynamicWaveTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             testRunTimeErrors   ();

  void             testSimpleInput     ();

  static boost::unit_test::test_suite*     suite               ();

};

} // namespace calc

#endif
