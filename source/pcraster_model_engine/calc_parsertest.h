#ifndef INCLUDED_CALC_PARSERTEST
#define INCLUDED_CALC_PARSERTEST



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif

// PCRaster library headers.

// Module headers.



namespace boost {
  namespace unit_test {
    class test_suite;
  }
}

namespace com {
  // Parser declarations.
  class Exception;
}



namespace calc {



//! This class implements the unit tests for the Parser class.
class ParserTest
{
  std::string model(const std::string& msgId) const;
public:

                   ParserTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             testExpr            ();
  void             testAssignment      ();
  void             testStatement       ();
  void             testCheckAndRewriteParsedAST();
  void             testStatementList   ();
  void             testCode            ();
  void             testModel           ();
  void             testBinding         ();
  void             testReportSection   ();
  void             testExternalBindings();
  void             testParseErrors     ();
  void             testNonAsciiScript  ();

  static boost::unit_test::test_suite*     suite               ();

};

} // namespace calc

#endif
