#ifndef INCLUDED_CALC_BUILDTYPESVISITORTEST
#define INCLUDED_CALC_BUILDTYPESVISITORTEST



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
#endif
// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_CALC_ASTTESTFACTORY
#include "calc_asttestfactory.h"
#define INCLUDED_CALC_ASTTESTFACTORY
#endif



namespace boost {
  namespace unit_test {
    class test_suite;
  }
}

namespace calc {
  // BuildTypesVisitor declarations.
  class ASTSymbolTable;
  class DataType;
  class ASTPar;
}



namespace calc {



//! This class implements the unit tests for the BuildTypesVisitor class.
class BuildTypesVisitorTest: public ASTTestFactory
{
  static bool      expectError(const char *msgId);
  /*!
   * a test table to mimic a resolve already done, symbols do not
   * have to exists really
   */
  ASTSymbolTable         *d_inputTable;
  std::vector<ASTPar *>   d_tt_pars;

  void insertTestTable(const std::string& name, const DataType& ft);

public:

                   BuildTypesVisitorTest           ();
                  ~BuildTypesVisitorTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             testPar             ();
  void             testNumber          ();
  void             testExpr            ();
  void             testModel           ();
  void             testReportParsed    ();
  void             testErrorExpr       ();
  void             testArgCombError    ();
  void             testAssError        ();
  void             testNonFieldError   ();
  void             testMultipleVisits  ();
  void             testNumberTyping    ();
  void             testDoubleFuncRelic ();
  void             testRepeat          ();

  void             testTopDownExprRestrictor();


  static boost::unit_test::test_suite*     suite               ();

};

} // namespace calc

#endif
