#ifndef INCLUDED_CALC_RUNTIMEENGINETEST
#define INCLUDED_CALC_RUNTIMEENGINETEST



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
  // RunTimeEngine declarations.
}



namespace calc {



//! This class implements the unit tests for the RunTimeEngine class.
class RunTimeEngineTest
{

private:

public:

                   RunTimeEngineTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             testPopField        ();
  void             testCloneSet        ();
  void             testCloneDiffer     ();
  void             testNrArgs          ();
  void             testTypeCheck       ();
  void             testResetVs         ();
  void             testBuildExpr       ();
  void             testLoadByStorageId ();

  static boost::unit_test::test_suite* suite();

};

} // namespace calc

#endif
