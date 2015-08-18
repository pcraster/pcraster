#ifndef INCLUDED_FIELDAPI_READONLYSPATIALTEST
#define INCLUDED_FIELDAPI_READONLYSPATIALTEST



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
  // ReadOnlySpatial declarations.
}



namespace fieldapi {



//! This class implements the unit tests for the ReadOnlySpatial class.
class ReadOnlySpatialTest
{

public:

                   ReadOnlySpatialTest ();

  void             testAll             ();
  template<typename UseAsT,typename StoredAsT>
      void         testType            ();

  static boost::unit_test::test_suite*    suite();

};

} // namespace fieldapi

#endif
