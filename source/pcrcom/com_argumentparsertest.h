#ifndef INCLUDED_COM_ARGUMENTPARSERTEST
#define INCLUDED_COM_ARGUMENTPARSERTEST



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
  // ArgumentParser declarations.
}



namespace com {



//! This class implements the unit tests for the ArgumentParser class.
class ArgumentParserTest
{

private:

public:

                   ArgumentParserTest  ();

  void             setUp               ();

  void             tearDown            ();

  void             testString          ();

  void             testSize_t          ();

  void             testInt             ();

  void             testDouble          ();

  static boost::unit_test::test_suite* suite();

};

} // namespace com

#endif
