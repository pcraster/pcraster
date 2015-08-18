#ifndef INCLUDED_CALC_COMPLETEPARSER
#define INCLUDED_CALC_COMPLETEPARSER



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_CALC_PARSERINPUT
#include "calc_parserinput.h"
#define INCLUDED_CALC_PARSERINPUT
#endif
#ifndef INCLUDED_PARSER
#include "tokens.h"
#include "Parser.h"
#define INCLUDED_PARSER
#endif

#ifndef INCLUDED_CALC_REWRITEPARSEDAST
#include "calc_rewriteparsedast.h"
#define INCLUDED_CALC_REWRITEPARSEDAST
#endif


namespace calc {
  // CompleteParser declarations.
  class ASTScript;
}



namespace calc {


/*! parse Grammar Construct yielding a GC object from complete parser input supplied
 *  by PI_CTOR that is the  ctor argument of a calc::ParserInput:
 *   - GC: GrammarConstruct, return value of pmem: public method of Parser, e.g. Parser::model()
 *   - PI_CTOR: ParserInput,
 *       - string as input
 *       - PathName for file contents as input
 *       - LexInputCreator for lexer
 *
 *  Many methods can serve as the pmem argument of the parse method's.
 *  Most are used only for unit-testing purposes.
 *
 * Complete parser input means that the parse method is expected to consume ALL
 * input (hence Complete) provided by PI_CTOR, not just a part until GC is parsed
 * leaving input after completing/satisfying the grammar construct GC.
 *
 * Only this class can parse, including the checkAndRewriteParsedAST(ASTNode *n);
 *
 */
template
 < typename GC,
   typename PI_CTOR=const std::string // ParserInput, string or PathName for file
 >
class CompleteParser
{

private:

  //! Assignment operator. NOT IMPLEMENTED.
  CompleteParser&           operator=           (const CompleteParser& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   CompleteParser               (const CompleteParser& rhs);

  typedef  std::auto_ptr<GC>(Parser::* PMEM_AP)(int *retsignal);
  typedef  void             (Parser::* PMEM_REF)(int *retsignal, GC& ref);

  ParserInput  d_pi;
  Parser       d_parser;

  void finish() {
    int retsignal;
    // endOfInput will throw if something else than end-of-input is left
    d_parser.endOfInput(&retsignal);
  }

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  //! ctor
  /*!
   * \param s a constructor for the ParserInput class
   */
  CompleteParser(const PI_CTOR& s):
    d_pi(s),
    d_parser(d_pi.tokenBuffer())
  {
    d_parser.initialize();
  }

  /* virtual */    ~CompleteParser              ()
  {
  }

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //! parse for GC's returning ptr to created object (callee must delete)
  /*!
   * \param pmem any of the public method of Parser, e.g. Parser::model()
   */
  GC* parse(PMEM_AP pmem) {
    int retsignal;
    std::auto_ptr<GC> n((d_parser.*pmem)(&retsignal));
    finish();
    checkAndRewriteParsedAST(n.get());
    return n.release();
  }

  //! parse for GC's accepting reference to GC
  /*!
   * \param pmem any of the public method of Parser, e.g. Parser::model()
   */
  void parse(PMEM_REF pmem, GC& ref) {
    int retsignal;
    (d_parser.*pmem)(&retsignal,ref);
    checkAndRewriteParsedAST(&ref);
    finish();
  }

  GC *parseScript() {
    int retsignal;
    std::auto_ptr<GC> script(d_parser.model(&retsignal));
    checkAndRewriteParsedAST(script->astCode());
    finish();
    return script.release();
  }

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

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
