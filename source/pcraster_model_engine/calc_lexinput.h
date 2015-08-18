#ifndef  INCLUDED_CALC_LEXINPUT
#define INCLUDED_CALC_LEXINPUT

#ifndef INCLUDED_OSTREAM
#include <iostream>
#define INCLUDED_OSTREAM
#endif
#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
#endif
#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif

namespace com {
  class PathName;
}

namespace calc {

class LexInputSource;
class Position;

//! input object for use in script lexer/parser
/*! LexInput implements a character stream initiated from
 *  either an input (script) file or a statement from the command line.
 *  It also implements the shell-$ substitution
 *  <BR>Still to do<UL>
 *  <LI>Make InstallCommandLineScript and InstallScript ctor's to elimnate d_from
 *      that could now be intialized more then once
 *  <LI>rewrite shell subst.
 *  </UL>
 *  \warning
 */
class LexInput {
private:
  //! last call to GetChar return newline
  bool d_prevCallWasNewLine;
  //! last call to GetChar is in comment
  bool d_gettingInComment;

  //! line nr is kept
  int d_lineNr;
  //! char nr  relative to beginning of line, begin of token
  int d_tokenStart;

  //! buffer used to parse macro
  std::string d_expInBuf;

  //! buffer used to store expanded macro's and feed it to parser
  std::string d_expOutBuf;
  /*! next char to feed, if not a valid index then do not feed
      from d_expOutBuf;
   */
  size_t   d_ptrExpOutBuf;

  LexInputSource *d_from;

  std::vector<std::string> d_shellArgs;

  int    d_extraCharRead;
  bool   d_substitution;

  int getRawChar(void);
  char getRawCharEOFcheck(void);
  int getParameterNr(      const std::string& name, bool nIsShellArg);
  std::string getParameter(const std::string& name, bool bracePresent);

  void parseShellParamUse(void);

  //! not implemented
  LexInput(const LexInput&);
  //! not implemented
  LexInput& operator=(const LexInput&);

public:
  LexInput();
  ~LexInput();

  void printExpandedCode(std::ostream& outStream);
  void installArgvScript(int argC, const char **argV,bool substitution=true);
  void installFileScript(const com::PathName& fileName);
  void installStringScript(const char *str);
  void installShellArgs(int nrShellArgs_in, const char **argV);

  int       getChar();
  void      incrCharNr(size_t len) { d_tokenStart += len; }

  Position *createPosition() const;
  bool      substitutionOn() const { return d_substitution; }
};


}

#endif
