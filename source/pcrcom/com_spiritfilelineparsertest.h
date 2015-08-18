#ifndef INCLUDED_COM_SPIRITFILELINEPARSERTEST
#define INCLUDED_COM_SPIRITFILELINEPARSERTEST



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
  // SpiritFileLineParser declarations.
}



namespace com {



//! This class implements the unit tests for the SpiritFileLineParser class.
class SpiritFileLineParserTest
{

public:

                   SpiritFileLineParserTest();

  void             setUp               ();

  void             tearDown            ();

  void             testOne             ();

  void             testTwo             ();

  void             testEmpty           ();

  static boost::unit_test::test_suite* suite();

};

} // namespace com

#endif
