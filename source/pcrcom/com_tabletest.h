#ifndef INCLUDED_COM_TABLETEST
#define INCLUDED_COM_TABLETEST



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
  // Table declarations.
}



namespace com {



//! This class implements the unit tests for the Table class.
class TableTest
{

public:

                   TableTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             testTextFormat      ();

  static boost::unit_test::test_suite* suite();

};

} // namespace com

#endif
