#ifndef INCLUDED_LEXGRAMMAR
#define INCLUDED_LEXGRAMMAR

#undef   yyFlexLexer
#define  yyFlexLexer gramFlexLexer
#ifndef  INCLUDED_FLEXLEXER
#include <FlexLexer.h>
#define  INCLUDED_FLEXLEXER
#endif

#ifndef INCLUDED_CALC_LEXINPUT
#include "calc_lexinput.h"
#define INCLUDED_CALC_LEXINPUT
#endif

#ifndef INCLUDED_LEXTOKEN
#include "lextoken.h"
#define INCLUDED_LEXTOKEN
#endif

#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif
#ifndef INCLUDED_BOOST_NONCOPYABLE
#include <boost/noncopyable.hpp>
#define INCLUDED_BOOST_NONCOPYABLE
#endif

#include "ATokenStream.h"
namespace calc {
//! lexer for ANTLR based parser for the modelling language Grammar
class LexGrammar :
  public gramFlexLexer,
  public ANTLRTokenStream,
  public boost::noncopyable
{
private:

  LexInput&   d_input;

  std::string d_optionLine;

  bool        d_eofParsed;

  void parseComment();

  LexToken *createToken(ANTLRTokenType type, MAJOR_CODE op=OP_NOP, size_t snoopedChars=0);

  LexToken *createValue(ANTLRTokenType type, double nrValue);

  //! overwrite gramFlexLexer::LexerInput
  int LexerInput(char *buf, int max_size);

  ANTLRAbstractToken *idOrKeyWord();

  ANTLRAbstractToken *reference();

#ifdef DEBUG
  void checkSortedTable();
#endif

public:
  /*!
   * \todo
   *   convince myself that this will always return a an allocated token that
   *   must be deleted! Hence call it createToken!
   */
  ANTLRAbstractToken *getToken();

  LexGrammar(LexInput& input);

  const std::string& optionLine() const;

  bool eofParsed() const { return d_eofParsed; }

};
}
#endif
