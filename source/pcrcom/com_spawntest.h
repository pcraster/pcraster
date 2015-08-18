#ifndef INCLUDED_COM_SPAWNTEST
#define INCLUDED_COM_SPAWNTEST



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
  // Spawn declarations.
}



namespace com {



//! This class implements the unit tests for the Spawn class.
class SpawnTest
{

private:

public:

                   SpawnTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             testNoArg           ();

  void             testArgs            ();

  static boost::unit_test::test_suite* suite();

};

} // namespace com

#endif
