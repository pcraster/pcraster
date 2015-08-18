%{
#include "stddefx.h"

#include "misc.h" /* ReplaceDirPathDelimChar */

#ifndef INCLUDED_CALC_LEXVALUEFILE
#include "calc_lexvaluefile.h"
#define INCLUDED_CALC_LEXVALUEFILE
#endif

#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif

#ifndef INCLUDED_CALC_QUOTE
#include "calc_quote.h"
#define INCLUDED_CALC_QUOTE
#endif

#ifndef INCLUDED_SSTREAM
#include <sstream>
#define INCLUDED_SSTREAM
#endif

#ifndef INCLUDED_IOSTREAM
#include <iostream>
#define INCLUDED_IOSTREAM
#endif
using std::cerr;
using std::cin;
using std::cout;

%}

%option yylineno
%option yyclass="calc::LexValueFile"
%option noyywrap

digit  [0-9]
letter [a-zA-Z_]
ws     [ \t\r\n]
      /* white space */
sign   [-+]
integer    {digit}+
ldp        ({letter}|{digit}|[\.])
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

%%

[\*]    { return '*';  }
[\n]    { return '\n'; }
{id}    {
          /* printf("id_key %s at %d\n",yytext,globCharNr);  */
          ReplaceDirPathDelimChar(yytext);
          return(IT_ID); 
        }
{reference}  {
       /* printf("reference %s at %d\n",yytext,globCharNr);  */
       ReplaceDirPathDelimChar(yytext);
       return IT_REF;
       }
({sign})?{integer} {
         long value = atol(YYText());
         if (value < (long)INT_MIN || value > (long)INT_MAX)
            return IT_FLOAT;
         return IT_INT; 
        }

({sign})?{digit}+{Exp}                { return IT_FLOAT; }
({sign})?{digit}*"."{digit}+({Exp})?  { return IT_FLOAT; }
({sign})?{digit}+"."{digit}*({Exp})?  { return IT_FLOAT; }
{ws} { 
        ; /* ignore */
     }
"#"  {
        return YYcomment();
     }
.    {  // pcrcalc/test284
        std::ostringstream msg;
        msg << "Illegal sequence ";
        msg << quote(YYText());
        msg << " at line ";
        msg << lineno();
        throw com::Exception(msg.str());
     }

%%

calc::LexValueFile::LexValueFile(
  std::istream* file):
    yyFlexLexer(file)
{
}

//! Skips all characters in a comment
/*! Skips all characters in a comment including the comment
 */
int calc::LexValueFile::YYcomment(void)
{
  int c = 'x';
  while ( c != EOF && c != '\n')
  {
    c = yyinput();
    if (c == EOF)
      return EOF ;
    if (c == '\n')
      return '\n';
  }
  PRECOND(FALSE);
  return 0 ;
}


void calc::LexValueFile::process()
{
  Line line;
  int token=1;
  while(token) {
   int currentLineNr = lineno();
   token = yylex();
   switch(token) {
            case IT_INT:
      case IT_FLOAT:
      case IT_REF:
      case IT_ID: {
       std::string s = YYText();
       if (token == IT_REF)
        s = std::string(s.begin()+1,s.end()-1);
       line.push_back(s);
       break;
      }
      case 0:
      case '\n': {
        if (!line.size()) // nothing to process, empty line or eof with ending '\n'
         break;
        processLine(line,currentLineNr);
        line.clear();
      }
      default: // CW todo '*' and better checking
        ;
   } // eoswitch
  } // eowhile
}
