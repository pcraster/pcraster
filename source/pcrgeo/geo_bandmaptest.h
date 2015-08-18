#ifndef INCLUDED_GEO_BANDMAPTEST
#define INCLUDED_GEO_BANDMAPTEST



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
  // BandMap declarations.
}



namespace geo {



//! This class implements the unit tests for the BandMap class.
class BandMapTest
{

public:

                   BandMapTest         ();

  void             setUp               ();

  void             tearDown            ();

  void             testOpen            ();
  void             testOpen2           ();
  void             testMultiBand       ();
  void             testCreate          ();
  void             testRead            ();
  void             testPutCells        ();
  void             testHeader          ();
  void             testRasterSpace     ();

  static boost::unit_test::test_suite* suite();

};

} // namespace geo

#endif
