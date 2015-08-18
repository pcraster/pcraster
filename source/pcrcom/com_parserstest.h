#ifndef INCLUDED_COM_PARSERSTEST
#define INCLUDED_COM_PARSERSTEST



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
  // Parsers declarations.
}



namespace com {



//! This class implements the unit tests for the Parsers class.
class ParsersTest
{

public:

                   ParsersTest         ();

  void             setUp               ();

  void             tearDown            ();

  void             testCommentParser   ();

  void             testSectionHeaderParser();

  void             testNumberParser    ();

  void             testVariableNameParser();

  void             testFileNameParser  ();

  void             testFunctionCallParser();

  void             testExpressionParser();

  static boost::unit_test::test_suite* suite();

};

} // namespace com

#endif
