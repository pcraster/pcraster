#ifndef INCLUDED_CALC_CALC
#define INCLUDED_CALC_CALC



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_CALC_SCRIPT
#include "calc_script.h"
#define INCLUDED_CALC_SCRIPT
#endif
#ifndef INCLUDED_CALC_LEXINPUT
#include "calc_lexinput.h"
#define INCLUDED_CALC_LEXINPUT
#endif
#ifndef INCLUDED_CALC_EXECUTESCRIPTSTATUS
#include "calc_executescriptstatus.h"
#define INCLUDED_CALC_EXECUTESCRIPTSTATUS
#endif

namespace com {
  class PathName;
}

namespace calc {

class  ProgressCallBack;



//! Abstract class for a calc object
/*!
 *  A calc object is able to perform certains actions on a script
 */
class Calc
{
private:

  //! Assignment operator. NOT IMPLEMENTED.
  Calc&           operator=           (const Calc&);

  //! Copy constructor. NOT IMPLEMENTED.
                   Calc               (const Calc&);
  //! the script
  Script  d_script;

protected:
  Script&  script() { return d_script; }

   //! set in executeScript
  ExecuteScriptStatus  d_executeScriptStatus;

  //! the lexical analyzer
  LexInput d_lexInput;


  //! stream where std out message are written to
  std::ostream& d_stdOut;
  //! stream where error message and progress are written to
  std::ostream& d_stdErr;

  //! remove an esri grid instead of usual action
  bool d_esriGridKillHack;
  //! print the argument expansion ($) instead of usual action
  bool d_printShellExpansionOnly;
  //! only check the script on syntax and presence of all input instead of usual action
  bool d_testScriptRunableOnly;

  bool d_printProfileInfo;

protected:

  //! implement here what to call in run();
  /*!
      \returns preffered 'exit' code
   */
  virtual int execute()=0;

  bool processArgs(int   argc, char**argv);

  void parse();
  int  executeScript();

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   Calc     ( std::ostream& stdOut,
                              std::ostream& stdErr);

    virtual       ~Calc               ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void setScriptFile(const com::PathName& script);

  void setRunDirectory(const com::PathName&  rd);
  void setProgressCallBack(ProgressCallBack* pcb);

  int  run(int returnValueOnException=1);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
 ExecuteScriptStatus executeScriptStatus() const;
 bool printProfileInfo() const;


};



//------------------------------------------------------------------------------
// INLINE FUNCIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------
void globalInit();

} // namespace calc

#endif
