#ifndef INCLUDED_CALC_OPTIONS
#define INCLUDED_CALC_OPTIONS



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_CALC_RUNTIMEENVSETTINGS
#include "calc_runtimeenvsettings.h"
#define INCLUDED_CALC_RUNTIMEENVSETTINGS
#endif
#ifndef INCLUDED_CALC_LEXINPUTCREATOR
#include "calc_lexinputcreator.h"
#define INCLUDED_CALC_LEXINPUTCREATOR
#endif



namespace calc {
  // Options declarations.
}



namespace calc {

class LexInput;

//! Options, settings, etc.
/*!
 * \bug EndGetOpt and so on stinks: no 2 Options objects instantiated
 *      at once possible
 */
class Options : public RunTimeEnvSettings, public LexInputCreator
{

private:

  typedef enum SCRIPT_TYPE {
         SCRIPT_CMD_LINE, SCRIPT_SCRIPT_FILE,
         SCRIPT_SHELL_FILE, SCRIPT_ESRI_GRID_KILL
  } SCRIPT_TYPE;

  //! Assignment operator. NOT IMPLEMENTED.
  Options&           operator=           (const Options& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   Options               (const Options& rhs);

  //! size of d_argv
  int              d_argc;
  //! d_argv (and d_argc) is only set only processCmdLineOptions to get the specified command
  char**           d_argv;

  SCRIPT_TYPE      d_scriptType;

  void processArgs(int   argc, char**argv);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   Options               ();

  /* virtual */    ~Options              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------


  void             processOptionString   (const std::string& argsExclArg0);
  void             processCmdLineOptions (int   argc, char**argv);


  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  LexInput*        createLexInput        () const;
  static void      printUsage            ();

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
