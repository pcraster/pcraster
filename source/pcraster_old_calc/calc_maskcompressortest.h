#ifndef INCLUDED_CALC_MASKCOMPRESSORTEST
#define INCLUDED_CALC_MASKCOMPRESSORTEST



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
  // MaskCompressor declarations.
}



namespace calc {



//! This class implements the unit tests for the MaskCompressor class.
class MaskCompressorTest
{

public:

                   MaskCompressorTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             testCompressor      ();

  void             testScript          ();

  void             test0Option         ();

  static boost::unit_test::test_suite*     suite               ();

};

} // namespace calc

#endif
