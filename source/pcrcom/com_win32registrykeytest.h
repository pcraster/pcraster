#ifndef INCLUDED_COM_WIN32REGISTRYKEYTEST
#define INCLUDED_COM_WIN32REGISTRYKEYTEST



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
  // Win32RegistryKey declarations.
}



namespace com {



//! This class implements the unit tests for the Win32RegistryKey class.
class Win32RegistryKeyTest
{

public:

                   Win32RegistryKeyTest();

  void             setUp               ();

  void             tearDown            ();

  void             testCycle           ();
  void             testWIN32setting    ();

  static boost::unit_test::test_suite* suite();

};

} // namespace com

#endif
