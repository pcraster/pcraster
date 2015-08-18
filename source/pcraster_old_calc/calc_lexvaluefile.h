#ifndef INCLUDED_CALC_LEXVALUEFILE
#define INCLUDED_CALC_LEXVALUEFILE

// #undef   yyFlexLexer
// #define  yyFlexLexer LexValueFile
#ifndef  INCLUDED_FLEXLEXER
#include <FlexLexer.h>
#define  INCLUDED_FLEXLEXER
#endif

#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
#endif

#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif

namespace calc {

//! lexer to break up file in string tokens
/*! calc_LexValueFile scans a file, skipping #-comments
    and combining each string. "-quoted strings are kept
    as one string
*/
class LexValueFile : public yyFlexLexer {
private:
  //! symbol (plus '*' and '\n')
  static const int IT_ID =1;
  static const int IT_REF=2;
  static const int IT_INT=3;
  static const int IT_FLOAT=4;
  int yylex();
  int YYcomment();
protected:
  typedef std::vector<std::string> Line;
  virtual void processLine(const Line& line,int lineNr) =0;
  LexValueFile(std::istream *file);
public:
  void process();
};

}

#endif
