#ifndef INCLUDED_COM_KEYVALUETABLETEST
#define INCLUDED_COM_KEYVALUETABLETEST



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
  // KeyValueTable declarations.
}



namespace com {



//! This class implements the unit tests for the KeyValueTable class.
class KeyValueTableTest
{

public:

                   KeyValueTableTest   ();

  void             setUp               ();

  void             tearDown            ();

  void             testAdd             ();
  void             testEnum            ();
  void             testInteger         ();
  void             testDouble          ();
  void             testRequired        ();

  static boost::unit_test::test_suite* suite();

};

} // namespace com

#endif
