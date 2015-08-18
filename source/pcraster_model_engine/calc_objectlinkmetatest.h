#ifndef INCLUDED_CALC_OBJECTLINKMETATEST
#define INCLUDED_CALC_OBJECTLINKMETATEST



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

namespace calc {
  // ObjectLinkMeta declarations.
}



namespace calc {



//! This class implements the unit tests for the ObjectLinkMeta class.
class ObjectLinkMetaTest
{

private:

public:

                   ObjectLinkMetaTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             testCtor            ();

  static boost::unit_test::test_suite* suite();

};

} // namespace calc

#endif
