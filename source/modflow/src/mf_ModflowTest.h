#ifndef INCLUDED_MF_MODFLOWTEST
#define INCLUDED_MF_MODFLOWTEST

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

namespace mf {
  // ModflowTest declarations.
}



namespace mf {

//! This class implements the unit tests for the Modflow class.
class ModflowTest
{

private:

public:

                   ModflowTest           ();

  void             test                ();

  static boost::unit_test::test_suite* suite();

};

} // namespace mf

#endif
