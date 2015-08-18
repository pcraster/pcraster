#ifndef INCLUDED_CALC_DATATYPETEST
#define INCLUDED_CALC_DATATYPETEST



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
  // DataType declarations.
}



namespace calc {



//! This class implements the unit tests for the DataType class.
class DataTypeTest
{

private:

public:

                   DataTypeTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             testCtor            ();
  void             testRestrict        ();

  static boost::unit_test::test_suite* suite();

};

} // namespace calc

#endif
