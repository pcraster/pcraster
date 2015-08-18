#ifndef INCLUDED_COM_RLEPTRVECTORTEST
#define INCLUDED_COM_RLEPTRVECTORTEST



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
  // RLEPtrVector declarations.
}



namespace com {



//! This class implements the unit tests for the RLEPtrVector class.
class RLEPtrVectorTest
{

public:

                   RLEPtrVectorTest    ();

  void             setUp               ();

  void             tearDown            ();

  void             testRLEItem         ();

  void             testPushBack        ();

  void             testIterator        ();

  static boost::unit_test::test_suite* suite();

};

} // namespace com

#endif
