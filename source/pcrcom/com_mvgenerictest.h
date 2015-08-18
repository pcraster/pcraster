#ifndef INCLUDED_COM_MVGENERICTEST
#define INCLUDED_COM_MVGENERICTEST



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
  // MVGeneric declarations.
}



namespace com {



//! This class implements the unit tests for the MVGeneric class.
class MVGenericTest
{

public:

                   MVGenericTest       ();

  void             setUp               ();

  void             tearDown            ();

  void             testVisitNonMV      ();

  void             testIterator        ();
  void             testSpatialNonSpatialIterate();
  void             testSpatialNonSpatialForEach();

  static boost::unit_test::test_suite* suite();

};

} // namespace com

#endif
