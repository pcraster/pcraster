#ifndef INCLUDED_PCRCALCPY_PYTHONUNITTEST
#define INCLUDED_PCRCALCPY_PYTHONUNITTEST



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.

// Module headers.



//class Test;

namespace boost {
  namespace unit_test {
    class test_suite;
  }
}


namespace pcrcalcpy {
  // PythonUnit declarations.
}



namespace pcrcalcpy {



//! This class implements the unit tests for the PythonUnit class.
class PythonUnitTest
{

private:

public:

                   PythonUnitTest      ();

  void             test                ();

  void             dummy               ();

  static boost::unit_test::test_suite* suite();

};

} // namespace pcrcalcpy

#endif
