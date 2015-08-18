#ifndef INCLUDED_BLOCK_NEWCLASSTEST
#define INCLUDED_BLOCK_NEWCLASSTEST



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

namespace block {
  // NewClassTest declarations.
}



namespace block {

//! This class implements the unit tests for the NewClass class.
class NewClassTest
{

private:

public:

                   NewClassTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             test                ();

  static boost::unit_test::test_suite* suite();

};

} // namespace block

#endif
