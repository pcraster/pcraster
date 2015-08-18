#ifndef INCLUDED_COM_SINGLEVALUEDRASTERTEST
#define INCLUDED_COM_SINGLEVALUEDRASTERTEST



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

namespace com {
  // SingleValuedRaster declarations.
}



namespace com {



//! This class implements the unit tests for the SingleValuedRaster class.
class SingleValuedRasterTest
{

public:

                   SingleValuedRasterTest();

  void             setUp               ();

  void             tearDown            ();

  void             testThis            ();

  void             testIRaster         ();

  static boost::unit_test::test_suite* suite();

};

} // namespace com

#endif
