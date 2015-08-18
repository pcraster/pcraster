#ifndef INCLUDED_CALC_STRINGPARSER
#define INCLUDED_CALC_STRINGPARSER

#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.


// Module headers.
#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif


namespace calc {
  // StringParser declarations.
  class ASTNode;
  class ASTNodeVector;
  class ASTNodeList;
  class ASTAss;
  class ASTStat;
  class ASTScript;
  class Code;
}


namespace calc {


/*!
   \brief
    Interface to the ANLTR generated class Parser, that returns an AST-tree
    with grammar construct in a string.

    Each (sensible) grammar construct has a counterpart method here, e.g.
    Parser::expr has StringParser::createExpr(const std::string& s).

    Every method will throw if the string has more characters (except whitespace,
    comments) then needed for the construct.

    Tested in  calc_parsertest.cc
 */
class StringParser
{

private:

  //! Assignment operator. NOT IMPLEMENTED.
  StringParser&           operator=           (const StringParser&);

  //! Copy constructor. NOT IMPLEMENTED.
                   StringParser               (const StringParser&);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   StringParser               ();

  /* virtual */    ~StringParser              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------


  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  static ASTNode*     createExpr              (const std::string& s);
  static ASTAss*      createAssignment        (const std::string& s);
  static ASTStat*     createStatement         (const std::string& s);
  static ASTNodeList* createStatementList     (const std::string& s);
  static Code*        createCode              (const std::string& s);
  static ASTNode*     createCodeAsNode        (const std::string& s);

  static ASTScript*   createScript             (const std::string& str);
};



//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace calc

#endif
