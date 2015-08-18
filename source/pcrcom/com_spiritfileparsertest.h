#ifndef INCLUDED_COM_SPIRITFILEPARSERTEST
#define INCLUDED_COM_SPIRITFILEPARSERTEST



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
  // SpiritFileParser declarations.
}



namespace com {



//! This class implements the unit tests for the SpiritFileParser class.
class SpiritFileParserTest
{

public:

                   SpiritFileParserTest();

  void             setUp               ();

  void             tearDown            ();

  void             testCurrent         ();

  void             fileMapTooLarge     ();

  static boost::unit_test::test_suite* suite();

};

} // namespace com

#endif
