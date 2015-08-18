#ifndef INCLUDED_FUNC_ASSIGNTEST
#define INCLUDED_FUNC_ASSIGNTEST



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

namespace func {
  // Assign declarations.
}



namespace func {



//! This class implements the unit tests for the Assign class.
class AssignTest
{

private:

public:

                   AssignTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             test                ();

  static boost::unit_test::test_suite* suite();

};

} // namespace func

#endif
