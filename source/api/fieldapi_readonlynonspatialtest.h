#ifndef INCLUDED_FIELDAPI_READONLYNONSPATIALTEST
#define INCLUDED_FIELDAPI_READONLYNONSPATIALTEST



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




namespace fieldapi {



//! This class implements the unit tests for the ReadOnlyNonSpatial class.
class ReadOnlyNonSpatialTest
{
public:

                   ReadOnlyNonSpatialTest           ();

  template<typename T> void testType   (T initVal);
  void             testAll             ();

  static boost::unit_test::test_suite* suite();

};

} // namespace fieldapi

#endif
