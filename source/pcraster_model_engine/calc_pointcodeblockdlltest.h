#ifndef INCLUDED_CALC_POINTCODEBLOCKDLLTEST
#define INCLUDED_CALC_POINTCODEBLOCKDLLTEST



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
  // PointCodeBlockDll declarations.
}



namespace calc {



//! This class implements the unit tests for the PointCodeBlockDll class.
class PointCodeBlockDllTest
{

private:

public:

                   PointCodeBlockDllTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             testIfThenElse      ();

  void             testCompile         ();
  void             test_f              ();

  static boost::unit_test::test_suite* suite();

};

} // namespace calc

#endif
