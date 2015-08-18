#ifndef INCLUDED_COM_TABLEINFOTEST
#define INCLUDED_COM_TABLEINFOTEST



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
  // TableInfo declarations.
}



namespace com {



//! This class implements the unit tests for the TableInfo class.
class TableInfoTest
{

public:

                   TableInfoTest       ();

  void             setUp               ();

  void             tearDown            ();

  void             testDetect          ();

  void             testBigDetect       ();

  void             testAvailable       ();

  static boost::unit_test::test_suite* suite();

};

} // namespace com

#endif
