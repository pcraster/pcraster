#ifndef INCLUDED_COM_TEMPDIRECTORYTEST
#define INCLUDED_COM_TEMPDIRECTORYTEST



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
  // TempDirectory declarations.
}



namespace com {



//! This class implements the unit tests for the TempDirectory class.
class TempDirectoryTest
{

private:

public:

                   TempDirectoryTest   ();

  void             setUp               ();

  void             tearDown            ();

  void             testCtorDtor        ();
  void             testRemoveFailure   ();

  static boost::unit_test::test_suite* suite();

};

} // namespace com

#endif
