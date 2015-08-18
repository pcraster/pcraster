#ifndef INCLUDED_FUNC_SETMVTEST
#define INCLUDED_FUNC_SETMVTEST



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
  // SetMVTest declarations.
}



namespace func {

//! This class implements the unit tests for the SetMV class.
class SetMVTest
{

private:

public:

                   SetMVTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             test                ();

  static boost::unit_test::test_suite* suite();

};

} // namespace func

#endif
