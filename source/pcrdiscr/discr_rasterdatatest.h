#ifndef INCLUDED_DISCR_RASTERDATATEST
#define INCLUDED_DISCR_RASTERDATATEST



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

namespace discr {
  // RasterData declarations.
}



namespace discr {



//! This class implements the unit tests for the RasterData class.
class RasterDataTest
{

private:

public:

                   RasterDataTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             testConstructor     ();

  void             testEquals          ();

  void             testAssignmentOperator();

  static boost::unit_test::test_suite* suite();

};

} // namespace discr

#endif
