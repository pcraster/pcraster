#include "stddefx.h"

/********/
/* USES */
/********/

/* libs ext. <>, our ""  */
#include <stdio.h>    /* EOF */
#include "misc.h"
/* global header (opt.) and lexinput's prototypes "" */
#include "calc_lexinput.h"

#ifndef INCLUDED_CSTRING
#include <cstring>
#define INCLUDED_CSTRING
#endif

#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif
#ifndef INCLUDED_COM_STRLIB
#include "com_strlib.h"
#define INCLUDED_COM_STRLIB
#endif
#ifndef INCLUDED_COM_PATHNAME
#include "com_pathname.h"
#define INCLUDED_COM_PATHNAME
#endif

#ifndef INCLUDED_CALC_QUOTE
#include "calc_quote.h"
#define INCLUDED_CALC_QUOTE
#endif

#ifndef INCLUDED_SSTREAM
#include <sstream>
#define INCLUDED_SSTREAM
#endif

#ifndef INCLUDED_CALC_LEXINPUTSOURCE
#include "calc_lexinputsource.h"
#define INCLUDED_CALC_LEXINPUTSOURCE
#endif
#ifndef INCLUDED_CALC_LEXINPUTSOURCESTRING
#include "calc_lexinputsourcestring.h"
#define INCLUDED_CALC_LEXINPUTSOURCESTRING
#endif
#ifndef INCLUDED_CALC_POSITIONTEXT
#include "calc_positiontext.h"
#define INCLUDED_CALC_POSITIONTEXT
#endif

/* headers of this app. modules called */


int calc::LexInput::getRawChar()
{
  // if expanded code
  // then return expanded code
  if(d_ptrExpOutBuf < d_expOutBuf.size())
     return d_expOutBuf[d_ptrExpOutBuf++];

  d_expOutBuf="";
  d_ptrExpOutBuf=std::string::npos;

  if(d_extraCharRead != EOF) {
    int c = d_extraCharRead;
    d_extraCharRead = EOF;
    return c;
  }
  return d_from->getChar();
}

char calc::LexInput::getRawCharEOFcheck()
{
  int c = getRawChar();
  // args/test2a
  if (c == EOF)
    throw com::Exception("end-of-file in substitution "+quote("${"+d_expInBuf));
  return (char)c;
}

int calc::LexInput::getParameterNr(
        const std::string& name,
        bool nIsShellArg)
{
  int r;
  if (CnvrtInt(&r,name.c_str())) {
    if (r > 0 && r <= (int)d_shellArgs.size())
      return r;
    return -1;
  }
  if (nIsShellArg && name == "n" && d_shellArgs.size() >= 1)
    return d_shellArgs.size();
  return 0;
}

std::string calc::LexInput::getParameter(
  const std::string& name,
        bool bracePresent)
{
  // simply return "" in case of failure
  int r = getParameterNr(name,bracePresent);
  switch(r) {
   case -1: /* number out of range */
     return "";
   case  0: /* env. variable */
     { char *n = getenv(name.c_str());
       if (n)
         return n;
     }
     return "";
   default:
    PRECOND(r > 0 && r <= (int)d_shellArgs.size());
    // do not allow rec. subst. checked earlier
    //  argscalc/test84
    PRECOND(d_shellArgs[r-1][0] != '$');
    return d_shellArgs[r-1];
  }
}

calc::LexInput::LexInput():
  d_prevCallWasNewLine(false),
  d_gettingInComment(false),
  d_lineNr(1),
  d_tokenStart(1),
  d_ptrExpOutBuf(std::string::npos),
  d_from(0),
  d_extraCharRead(EOF),
  d_substitution(true)
{
}

calc::LexInput::~LexInput()
{
  delete d_from;
}

//! get a character from the input source
/*! called by calc::LexGrammar::LexerInput to get a char
 * <BR><B>OLD NOTE CODE NOT ACTIVE:</B>it's terrible but the endToken will be
 * in an end-comment break it's the dos problem of not putting a new-line
 * before the end of file (Norton editor) for example
 */
int calc::LexInput::getChar()
{
  if (d_prevCallWasNewLine) {
   d_prevCallWasNewLine=false;
   d_tokenStart=1;
   d_lineNr++;
  }
  int c=getRawChar();
  if (d_gettingInComment && c != '\n')
    return c;
  switch(c) {
   case '#' :
      d_gettingInComment=true;
      break;
   case '$' :
      if (substitutionOn()) {
         // install the substitution engine
         parseShellParamUse();
         // call again
         return getChar();
      }
      // otherwise return this char
      break;
   case '\n':
      d_gettingInComment=false;
      d_prevCallWasNewLine=true;
  }
  return c;
}

//! print expanded code to stdout (-t option)
/*!
 */
void calc::LexInput::printExpandedCode(
  std::ostream& outStream)
{
  int c;
  do {
   c =getChar();
   if (c != EOF)
     outStream.put(c);
  } while (c != EOF);
}


//! install <I>script</I> given on the command line
/*!
  \param argC number of elements in \a argV
  \param argV  arguments that contains  the calc expression in argv-style; except that
   entry 0 <b>is</b> used;it is not the name of the application as in main().
   contents is copied into the object
  \param substitution disabling substitution is needed for sealing $-constructs.
*/
void calc::LexInput::installArgvScript(
    int          argcIn,
    const char **argV,
    bool         substitution)
{
  d_substitution=substitution;

  PRECOND(argcIn >= 1);

  /* don't check the last, since we do not
   * have shell options then
   */

  int argc = argcIn;
  for (int i = 0; i < argc-1; i++)
   if (strstr(argV[i], ";;") != NULL) {
     // from here on the rest are d_shellArgs:
     installShellArgs(argc-i-1, argV+i+1);
     // till here it is the expression
     argc = i+1;
     break;
  }
  d_from = new LexInputSourceString(argc,argV);
}

void calc::LexInput::installStringScript(
    const char *str)
{
  installArgvScript(1,&str,false);
}

//! Install script given as option (-f)
void calc::LexInput::installFileScript(const com::PathName& fileName)
{
  d_from = createLexInputSourceFromFile(fileName);
}

//! install the arguments for shell substitution
/*!
 * \param nrArgV nr in \a argV
 * \param argV   argv style argument, data is copied.
 * \todo
 *   do we want recursive substitution?  argscalc/test84
 */
void calc::LexInput::installShellArgs(
  int nrArgV,
  const char **argV)
{
  d_shellArgs.clear();
  for (size_t i = 0; i < (size_t)nrArgV; i++) {
    d_shellArgs.push_back(argV[i]);
    if (d_shellArgs.back()[0] == '$') {
      std::ostringstream msg; // argscalc/test84
      msg << "recursive substitution value: " << d_shellArgs[i] << " is illegal";
      throw com::Exception(msg.str());
    }
  }
}

/*! set up expansion, all white space is already
 * throws Excep in case of error
 */
void calc::LexInput::parseShellParamUse()
{
   // scan entire $-construct
   // but do not include $ and { } if present

   d_expInBuf="";
   // DO NOT SET  d_ptrExpOutBuf = 0;
   // since we that enable feeding from d_expInBuf while
   // filling d_expInBuf
   d_ptrExpOutBuf = std::string::npos;
   d_extraCharRead = EOF;

   // symbol after $
   char c = getRawCharEOFcheck();
   if (c == '{') {
       // start of {expr}
       do {
         c = getRawCharEOFcheck();
         if (c == '}')
           break;
         d_expInBuf += c;
       } while(1);
   } else {
       // start of number or id (env. var.)
       do {
           if (!IsAlphaNumericUnderscore(c))
             break;
           d_expInBuf += c;
           c = getRawCharEOFcheck();
       } while(1);
       d_extraCharRead = c;
   }
   if (d_expInBuf.empty()) // args/test3
      throw com::Exception("no parameter name after $-symbol");


   if (d_expInBuf.find_first_of(",") == std::string::npos) {
     // have parsed single argument number or id
     // single argument
     PRECOND(d_expInBuf.find_first_of("{}$") == std::string::npos);
     if (com::equalNoSpace(d_expInBuf,""))  // "${}" possible
        throw com::Exception("no parameter name in ${name}-construct");
     d_expOutBuf=getParameter(d_expInBuf, d_extraCharRead==EOF);
     d_ptrExpOutBuf=0;
     return; // DONE
   }

   // retain space in sake of constructs like
   //  ${1,n, and , not $ }
   // - in argument 0 and 1 we want space stripped
   std::vector<std::string> args(com::split(d_expInBuf,','));
   // - the wrapper arg # 3 may contain , chars
   // jus tput them back in the wrapper arg
   for(size_t i=4; i < args.size(); i++)
     args[3] += ","+args[i];

   // expansion with seperator and wrapper, since we have ','
   // in the string
   PRECOND(args.size() > 0);
   com::removeAllSpace(args[0]);
   if (args[0].empty())
     throw com::Exception("Empty argument in substitution");
   int startPar = getParameterNr(args[0],true);
   if (args.size() <= 1)
     throw com::Exception("Empty argument in substitution");
   com::removeAllSpace(args[1]);
   if (args[1].empty())
     throw com::Exception("Empty argument in substitution");
   int endPar = getParameterNr(args[1],true);

   d_ptrExpOutBuf=0;
   if (startPar < 1 || endPar < 1 || startPar > endPar)
     return; // finished, quitly ignore mess

   std::string seperator(",");
   if (args.size() > 2) seperator=args[2];
   std::string wrapper("$");
   if (args.size() > 3) wrapper=args[3];
   for(int i=startPar-1; i <= endPar-1; i++) {
    d_expOutBuf+=com::replaceCharByStr(wrapper,'$', d_shellArgs[i]);
    if (i != endPar-1)
      d_expOutBuf+=seperator;
   }
}

calc::Position  *calc::LexInput::createPosition() const
{
  return d_from->createPositionText(d_lineNr,d_tokenStart);
}
