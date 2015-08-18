#ifndef INCLUDED_COM_MVOPTEST
#define INCLUDED_COM_MVOPTEST



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
  // MVOp declarations.
}



namespace com {



//! This class implements the unit tests for the MVOp class.
class MVOpTest
{

private:

public:

                   MVOpTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             testStaticAsserts   ();
  template<typename T>
    void           testOp              ();
  void             testImplicitCastStaticAsserts ();
  void             testStaticAss       ();

  static boost::unit_test::test_suite* suite();

};

} // namespace com

#endif
