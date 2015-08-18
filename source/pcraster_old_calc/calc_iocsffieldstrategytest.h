#ifndef INCLUDED_CALC_IOCSFFIELDSTRATEGYTEST
#define INCLUDED_CALC_IOCSFFIELDSTRATEGYTEST



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
  // IoCsfFieldStrategy declarations.
}



namespace calc {



//! This class implements the unit tests for the IoCsfFieldStrategy class.
class IoCsfFieldStrategyTest
{

public:

                   IoCsfFieldStrategyTest           ();

  void             setUp               ();

  void             tearDown            ();

  void          testGetStackReaderDefault();

  void          testGetStackReaderPath();

  static boost::unit_test::test_suite*     suite               ();

};

} // namespace calc

#endif
