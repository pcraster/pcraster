#ifndef INCLUDED_FIELDAPI_READWRITEDATATEST
#define INCLUDED_FIELDAPI_READWRITEDATATEST



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
  // ReadWriteData declarations.
}



namespace fieldapi {



//! This class implements the unit tests for the ReadWriteData class.
class ReadWriteDataTest
{

public:

                   ReadWriteDataTest           ();

  template<class UseAsT,class StoredAsT> void testType();

  void             testAll             ();

  static boost::unit_test::test_suite *    suite               ();

};

} // namespace fieldapi

#endif
