%{
#include "stddefx.h"

#include <string.h>

#ifndef INCLUDED_IOSTREAM
#include <iostream>
#define INCLUDED_IOSTREAM
#endif
using std::cerr;
using std::cin;
using std::cout;


#ifndef INCLUDED_MEMORY
#include <memory>
#define INCLUDED_MEMORY
#endif

#ifndef INCLUDED_LEXGRAMMAR
  // do not include renamed xxFlexLexer itself!
# define INCLUDED_FLEXLEXER
# include "lexgrammar.h"
# define INCLUDED_LEXGRAMMAR
#endif

#ifndef INCLUDED_PCRDATATYPE
#include "pcrdatatype.h"
#define INCLUDED_PCRDATATYPE
#endif

#ifndef INCLUDED_MAJOR_OP
#include "major_op.h"
#define INCLUDED_MAJOR_OP
#endif

#ifndef INCLUDED_MISC
#include "misc.h"
#define INCLUDED_MISC
#endif

#ifndef INCLUDED_APP
#include "app.h" // AppParseShellLine()
#define INCLUDED_APP
#endif

#ifndef INCLUDED_CALC_GLOBALLIBDEFS
#include "calc_globallibdefs.h"
#define INCLUDED_CALC_GLOBALLIBDEFS
#endif

#ifndef INCLUDED_CALC_POSITION
#include "calc_position.h"
#define INCLUDED_CALC_POSITION
#endif
#ifndef INCLUDED_CCTYPE
#include <cctype>
#define INCLUDED_CCTYPE
#endif

#include "tokens.h"
#undef  YY_DECL
#define YY_DECL ANTLRAbstractToken *calc::LexGrammar::getToken()


#define RETURN_TOK(token)              { return createToken(token);                  }
#define RETURN_OP(operator,groupName)  { return createToken(groupName,operator);     }
#define RETURN_INT(intValue,token)     { return createValue(token,intValue);         }
#define RETURN_FLOAT                   { return createValue(TOK_FLOAT,atof(yytext)); }

%}

%option yyclass="calc::LexGrammar"
%option prefix="gram"
%option noyywrap

digit  [0-9]
letter  [a-zA-Z_]
  /* letters and the underscore */

/*****************************
N U    dospath  [\$\%\'\`\@\{\}\~\#\&\\\.\:]
O S    doschar {letter}|{digit}|{dospath}
T E    filename  ({doschar})*
  D
*******************************/

ws    [ \t\r\n]
      /* white space */
integer    {digit}+
/*
 * id    ({letter}|[\.\\])({letter}|{digit}|[\.\\])*
 *                       * letter followed by 0 or more
 *                        * letter digits or points OR the DOS
 *                        * directory splitter!
 */
ldp             ({letter}|{digit}|[\.])
    /* letter digit or point
     */
directory       ({ldp})*[\\]+
idname          ({letter}{ldp}*)
id         ({letter}[\:])?({directory})*{idname}
reference  \"[^\"]+\"
                 /* a quote followed by 1 or more characters
        and ended by a quote */
Exp      [Ee][+-]?{digit}+
      /* expontential part of float */
repMomEndTime  \.\.endtime
repMomEndNr    \.\.{integer}

/* NB in next section:
 * C-code must start on same line as pattern
 * [\(\)\*\+\-\=\;\,\:\[\]\{\}] { RETURN_TOK(yytext[0]); }
 */

%%


\${integer}  { RETURN_TOK(TOK_DOLLAR); }
\(  { RETURN_TOK(TOK_LP); }
\)  { RETURN_TOK(TOK_RP); }
\[  { RETURN_TOK(TOK_LB); }
\]  { RETURN_TOK(TOK_RB); }
\{  { RETURN_TOK(TOK_LC); }
\}   { RETURN_TOK(TOK_RC); }
\*  { RETURN_OP(OP_MUL, TOK_STAR); }
\+  { RETURN_TOK(TOK_PLUS); }
\-  { RETURN_TOK(TOK_MINUS); }
\=  { RETURN_TOK(TOK_IS); }
\;  { RETURN_TOK(TOK_SC); }
\,  { RETURN_TOK(TOK_COMMA); }
"::"  { RETURN_TOK(TOK_2COL); }
"**"  {RETURN_TOK(TOK_POW); }
"&&"  {RETURN_TOK(TOK_AND); }
"||"   {RETURN_TOK(TOK_OR); }
"!"  { RETURN_OP(OP_NOT_, TOK_NOT);   }
"/"  { RETURN_OP(OP_FDIV,TOK_FDIV); }
">="  { RETURN_OP(OP_GE,  TOK_COMP); }
"<="  { RETURN_OP(OP_LE,  TOK_COMP); }
">"   { RETURN_OP(OP_GT,  TOK_COMP); }
"<"   { RETURN_OP(OP_LT,  TOK_COMP); }
"!="   { RETURN_OP(OP_NE,  TOK_EQ); }
"=="   { RETURN_OP(OP_EQ,  TOK_EQ); }
"*="  { RETURN_OP(OP_MUL, TOK_ASSOP); }
"/="  { RETURN_OP(OP_FDIV,TOK_ASSOP); }
"+="  { RETURN_OP(OP_BADD,TOK_ASSOP); }
"-="  { RETURN_OP(OP_BMIN,TOK_ASSOP); }
"**="  { RETURN_OP(OP_POW, TOK_ASSOP); }

{repMomEndTime} { RETURN_INT(-1,TOK_MOMENT_RANGE); }
{repMomEndNr}   { RETURN_INT((atoi((char *)yytext+2)),TOK_MOMENT_RANGE); }

{id}            { return idOrKeyWord(); }
{reference}     { return reference(); }

{integer}  |
{integer}/\.\.  { long value; /* right context .. (O'reilly p. 152) is used to get
                               * an integer definition for before the TOK_MOMENT_RANGE
                               * otherwise this lexer see 2. first as a TOK_FLOAT
                               * and trashes on the next point
                               */

                  value = atol((char *)yytext);
                  if (value < (long)INT_MIN || value > (long)INT_MAX)
                     RETURN_FLOAT;
                  RETURN_INT(atoi((char *)yytext),TOK_INT);
               }


{digit}+{Exp}                  { RETURN_FLOAT; }
{digit}*"."{digit}+({Exp})?    { RETURN_FLOAT; }
{digit}+"."{digit}*({Exp})?    { RETURN_FLOAT; }

{integer}{id}  { // pcrcalc/test343
                 // a common error starting an id with a number
                 // in lex-rules AFTER float, otherwise 1e32 will match here
                  RETURN_TOK(TOK_ILLEGALID);
               }

{ws}            { // skip whitespace
                   d_input.incrCharNr(strlen(yytext));
                }
"#"             { parseComment(); }
<<EOF>>         {
                  d_eofParsed = true;
                  RETURN_TOK(TOK_EOF); }
"\x1a"          {   // skip DOS EOF char
                     yyinput();
                }
.               { // pcrcalc/test318
                  RETURN_TOK(TOK_ILLEGAL);
                }

%%

calc::LexGrammar::LexGrammar(LexInput& input):
  d_input(input),
  d_eofParsed(false)
{
}

typedef struct KWORD
{
  const  char  *name;
    ANTLRTokenType  token;
    MAJOR_CODE  op;
} KWORD;

static const KWORD keywordTable[] = /* must be sorted */
{
  { "and"    , TOK_AND  , OP_NOP},
  { "areamap"  , TOK_AREAMAP  , OP_NOP},
  { "binding"  , TOK_BINDING  , OP_NOP},
  { "boolean"  , TOK_CONV_F  , OP_BOOLEAN_},
  { "directional"  , TOK_CONV_F  , OP_DIRECTIONAL},
  { "div"    , TOK_FDIV   , OP_FDIV},
  { "dynamic"  , TOK_DYNAMIC  , OP_NOP},
  { "else"  , TOK_ELSE  , OP_NOP},
  { "endtime"     , TOK_ENDTIME   , OP_NOP},
  { "eq"    , TOK_EQ  , OP_EQ},
  { "except"      , TOK_EXCEPT     , OP_NOP},
  { "fileoutput"  , TOK_FILEOUTPUT, OP_NOP},
  { "foreach"     , TOK_FOREACH   , OP_NOP},
  { "ge"    , TOK_COMP  , OP_GE},
  { "gt"    , TOK_COMP  , OP_GT},
  { "idiv"  , TOK_IDIV   , OP_IDIV},
  { "if"    , TOK_IF  , OP_NOP},
  { "in"    , TOK_IN  , OP_NOP},
  { "initial"   , TOK_INITIAL , OP_NOP},
  { "interface" , TOK_INTERFACE, OP_NOP},
  { "ldd"       , TOK_CONV_F  , OP_LDD},
  { "le"        , TOK_COMP    , OP_LE},
  { "lt"        , TOK_COMP    , OP_LT},
  { "mif"       , TOK_IF      , OP_NOP},
  { "mod"       , TOK_MOD     , OP_MOD},
  { "model"     , TOK_MODEL   , OP_NOP},
  { "ne"        , TOK_EQ      , OP_NE},
  { "nominal"   , TOK_CONV_F  , OP_NOMINAL},
  { "not"       , TOK_NOT     , OP_NOT_},
  { "object"    , TOK_OBJECT  , OP_NOP},
  { "or"        , TOK_OR      , OP_NOP},
  { "ordinal"   , TOK_CONV_F  , OP_ORDINAL},
  { "repeat"    , TOK_REPEAT  , OP_NOP},
  { "report"    , TOK_REPORT  , OP_NOP},
  { "scalar"    , TOK_CONV_F  , OP_SCALAR},
  { "then"      , TOK_THEN    , OP_NOP},
  { "timer"     , TOK_TIMER   , OP_NOP},
  { "until"     , TOK_UNTIL   , OP_NOP},
  { "xor"       , TOK_XOR     , OP_NOP}
};
#define NR_KWORDS  (ARRAY_SIZE(keywordTable))

static int cmp(const void *a, const void *b)
{
  return(strcmp(((const KWORD *)a)->name, ((const KWORD *)b)->name));
}

#ifdef DEBUG
void calc::LexGrammar::checkSortedTable(void)
/* verify if keywordTable is sorted correctly */
{
  static BOOL checked = FALSE;
  KWORD  table[NR_KWORDS];
  size_t i;

  if (! checked)
  {
    memcpy(table, keywordTable, sizeof(keywordTable));
    qsort(table, NR_KWORDS, sizeof(KWORD), cmp);
    for(i=0; i < NR_KWORDS; i++)
      POSTCOND(! cmp(table+i, keywordTable+i));
    checked = TRUE;
  }
}
#endif /* DEBUG */

/* Determine if something is a keyword or an id
 * PATH_SEPERATORS are in-situ changed to platform specifics
 * returns TOK_ID if it's an id or the appropriate
 * constant for the keyword (e.g. TOK_AND)
 */
ANTLRAbstractToken *calc::LexGrammar::idOrKeyWord()
{
  KWORD *p, dummy;

  IFDEBUG(checkSortedTable());

  dummy.name = yytext;
  p = static_cast<KWORD *>
     (bsearch((const void *)&dummy, (const void *)keywordTable,
      NR_KWORDS, sizeof(KWORD), cmp));

  if(p) {
    LexToken *lt=createToken(p->token,p->op);
    lt->setIsKeyword(true);
    return lt;
  }

  ReplaceDirPathDelimChar(yytext);
  RETURN_TOK(TOK_ID);
}

/*! parse id in a quoted reference, allowing path names etcetera
 * PATH_SEPERATORS are in-situ changed to platform specifics
 */
ANTLRAbstractToken *calc::LexGrammar::reference()
{
  // nr of chars in quoted string (minus 2 " )
  int i,n = strlen(yytext)-2;
  PRECOND(n >= 0); /* non-empty string plus end " */

  // remove "
  for(i=0; i<n ; i++)
    yytext[i] = yytext[i+1];
  yytext[i] = '\0';

  // replace PATH_SEPERATORS
  ReplaceDirPathDelimChar(yytext);

  // obsolete: now a number in quotes is a filename
  // if (com::isDouble(yytext)) {
  //   std::auto_ptr<Position> pos(d_input.createPosition());
  //   pos->throwError("filename can not be a numeric value");
  // }

  return createToken(TOK_REFERENCE,OP_NOP,2);
}

//! parse a comment
/*!
   if first comment with #! then process and/or store \a d_optionLine
 */
void calc::LexGrammar::parseComment()
{
  // save current position
  std::auto_ptr<Position> pos(d_input.createPosition());

  int c = yyinput();
  // if first time encountered
  if (c == '!' && d_optionLine.empty()) {
    // argscalc/test21b is empty line test
    // the starting #! for options
    while (c != EOF) {
      c = yyinput();
      if (c == EOF || c == '\n') {
         if (AppParseShellLine(d_optionLine.c_str())) {
           // argscalc/test21a
           pos->throwError(getLibError());
         }
         // add a space so it is never empty
         d_optionLine+=" ";
         return;
      }
      d_optionLine+=static_cast<char>(c);
    }
    POSTCOND(FALSE); // never reached
  }
  while (c != EOF && c != '\n')
    c = yyinput();
}

//! return parsed #! option, empty if none
const std::string& calc::LexGrammar::optionLine() const
{
    return d_optionLine;
}

//! attach calc::LexInput as input
int calc::LexGrammar::LexerInput(char *buf, int /* max_size */)
{
  int c = d_input.getChar();
  return (c == EOF) ? 0 : (buf[0] = c, 1);
}

calc::LexToken* calc::LexGrammar::createToken(
    ANTLRTokenType type,
    MAJOR_CODE op,
    size_t snoopedChars)
{
  // now snapshot where the token starts
  // befor updating new position
  Position* p=d_input.createPosition();
  // update new position
  d_input.incrCharNr(strlen(yytext)+snoopedChars);

  return new calc::LexToken(type, op, yytext, p);
}

calc::LexToken::LexToken(
  ANTLRTokenType type, MAJOR_CODE op, const char* text,
  Position *ownedPos):
    d_type(type), d_isKeyword(false),d_op(op),d_text(text), d_pos(ownedPos)
{
  try {
   switch(d_type) {
     case TOK_ILLEGALID:
       // pcrcalc/test343
       d_pos->throwError("Illegal combination of number and name: "+quote(d_text));
     case TOK_ILLEGAL:
       char aBuf[64];
       if (std::isprint(d_text[0])) {
         // pcrcalc/test318
         // argscalc/test41a
         // argscalc/test40
         d_pos->throwError("Unknown character(s) "+quote(d_text));
       }
       // pcrcalc/test375
       sprintf(aBuf,"not printable: %X(hex)",d_text[0]);
       d_pos->throwError(aBuf);
     default:;
   }
  } catch(...) {
     delete d_pos;
     throw;
  }
}

void calc::LexToken::setIsKeyword(bool isKeyword) {
    d_isKeyword=isKeyword;
}

bool calc::LexToken::isKeyword() const {
    return d_isKeyword;
}

calc::LexToken *calc::LexGrammar::createValue(ANTLRTokenType type, double nrValue)
{
  calc::LexToken *t = createToken(type);
  t->setNrValue(nrValue);
  return t;
}

calc::LexToken::~LexToken()
{
  delete d_pos;
}

//!
/*!
   \todo  make \a d_val and \a val integers
 */
void calc::LexToken::setNrValue(double val) {
    d_val=val;
}

ANTLRTokenType calc::LexToken::getType() const
{
 return d_type;
}

ANTLRChar     *calc::LexToken::getText() const
{
 // Brutal cast, Ok
 return (ANTLRChar *)d_text.c_str();
}

const std::string& calc::LexToken::stringVal()  const
{ return d_text;  
}

MAJOR_CODE calc::LexToken::opCode() const
{
  return d_op;
}

int calc::LexToken::integerVal() const
{
  return static_cast<int>(d_val);
}

const calc::Position *calc::LexToken::position() const
{
  return d_pos;
}

//! only needed by ANTLR to link
void calc::LexToken::setType(ANTLRTokenType /* t */) {
   PRECOND(FALSE); // Bollocks
}

//! only needed by ANTLR to link
void calc::LexToken::setText(const ANTLRChar * /* s */) {
    PRECOND(FALSE);// Bollocks
}

//! only needed by ANTLR to link
void calc::LexToken::setLine(int /* line */) {
    PRECOND(FALSE);// Bollocks
}
int calc::LexToken::getLine()const {
    PRECOND(FALSE);// Bollocks
    return 0;
}
