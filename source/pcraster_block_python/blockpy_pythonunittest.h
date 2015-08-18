#ifndef INCLUDED_BLOCKPY_PYTHONUNITTEST
#define INCLUDED_BLOCKPY_PYTHONUNITTEST



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

namespace blockpy {
  // PythonUnit declarations.
}



namespace blockpy {



//! This class implements the unit tests for the PythonUnit class.
class PythonUnitTest
{

private:

public:

                   PythonUnitTest      ();

  void             setUp               ();

  void             tearDown            ();

  void             test                ();

  static boost::unit_test::test_suite* suite();

};

} // namespace blockpy

#endif
