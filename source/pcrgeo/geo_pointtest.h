#ifndef INCLUDED_GEO_POINTTEST
#define INCLUDED_GEO_POINTTEST



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

namespace geo {
  // Point declarations.
}



namespace geo {



//! This class implements the unit tests for the Point class.
class PointTest
{

public:

                   PointTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             testLayout          ();

  void             testIndexDirection  ();

  void             testCloser          ();

  void             testDistance        ();

  static boost::unit_test::test_suite* suite();

};

} // namespace geo

#endif
