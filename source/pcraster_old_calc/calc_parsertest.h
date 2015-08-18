#ifndef INCLUDED_CALC_PARSERTEST
#define INCLUDED_CALC_PARSERTEST



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
  // Parser declarations.
}



namespace calc {



//! This class implements the unit tests for the Parser class.
class ParserTest
{

public:

                   ParserTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             testModelParser     ();

  static boost::unit_test::test_suite*     suite               ();

};

} // namespace calc

#endif
