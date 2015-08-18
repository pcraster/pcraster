#ifndef INCLUDED_CALC_OBJECTLINKTEST
#define INCLUDED_CALC_OBJECTLINKTEST



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
  // ObjectLink declarations.
}



namespace calc {



//! This class implements the unit tests for the ObjectLink class.
class ObjectLinkTest
{

private:

public:

                   ObjectLinkTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             testLoadLink        ();

  void             testLoadLink2       ();

  void             testExec            ();

  void             testExec2           ();

  void             testNoArguments     ();

  void             testCheckAndExec    ();

  static boost::unit_test::test_suite* suite();

};

} // namespace calc

#endif
