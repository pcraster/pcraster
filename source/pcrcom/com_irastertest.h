#ifndef INCLUDED_COM_IRASTERTEST
#define INCLUDED_COM_IRASTERTEST



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
  // IRaster declarations.
}



namespace com {



//! This class implements the unit tests for the IRaster class.
class IRasterTest
{

public:

                   IRasterTest         ();

  void             setUp               ();

  void             tearDown            ();

  void             testOutside         ();

  void             testGetCell         ();

  static boost::unit_test::test_suite* suite();

};

} // namespace com

#endif
