#ifndef INCLUDED_COM_APP
#define INCLUDED_COM_APP



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif

// PCRaster library headers.
#ifndef INCLUDED_PCRDLL
#include "pcrdll.h"
#define INCLUDED_PCRDLL
#endif

// Module headers.
#ifndef INCLUDED_COM_COMMANDLINE
#include "com_commandline.h"
#define INCLUDED_COM_COMMANDLINE
#endif

#ifndef INCLUDED_COM_COMMANDLINEARGUMENT
#include "com_commandlineargument.h"
#define INCLUDED_COM_COMMANDLINEARGUMENT
#endif

#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif



namespace com {

  typedef enum { UNKNOWN, GNU } License;

//! The App class is an abstract base class for applications.
/*!
  Applications can, for example, be command line applications or gui
  applications. This class implements the common stuff to all kinds of
  applications.

  The App class adds a few command line arguments to all sub classes. Make
  sure you know about them and in some cases, check if they are set. Don't
  use the same short/long argument names.

  <ul>
    <li>-h/--help: prints the usage information</li>
    <li>-v/--version: prints the version information</li>
    <li>--debug: file name for debug output</li>
    <li>--license: license information (optional, see constructor argument)</li>
  </ul>

  GNU is the only standard license the com lib knows about. When more licenses
  need to be supported add them here. User defined licenses can be supported
  by making licenseDescription() virtual and adding a USERDEFINED symbol to the
  License enumeration.

  If you don't want this class to handle command line parsing for you call
  setParseCommandLine(false). You can do your own parsing at the start of the
  exec function.
*/
class PCR_DLL_CLASS App
{

private:

  //! Argument count.
  int              d_argc;

  //! Argument vector.
  char**           d_argv;

  #if _MSC_VER // == 1400
    #error // check _MSC_VER
    #pragma warning(disable:4251)
  #endif
  //! Command line argument for help about the command.
  Option           d_helpArg;
  #if _MSC_VER // == 1400
    #pragma warning(default:4251)
  #endif

  #if _MSC_VER // == 1400
    #pragma warning(disable:4251)
  #endif
  //! Command line argument for version of the command.
  Option           d_versionArg;
  #if _MSC_VER // == 1400
    #pragma warning(default:4251)
  #endif

  #if _MSC_VER // == 1400
    #pragma warning(disable:4251)
  #endif
  //! Command line argument for license information.
  Option           d_licenseArg;
  #if _MSC_VER // == 1400
    #pragma warning(default:4251)
  #endif

#ifdef DEBUG_DEVELOP
  //! Command line argument for debug stream. Only in DEBUG_DEVELOP mode.
  OptionValue<std::string> d_debugStreamArg;
#endif

  //! Command line of application.
  CommandLine      d_cmdLine;

  //! Auto parse the command line or not.
  bool             d_parseCommandLine;

  //! License of this app.
  License          d_license;

  //! Assignment operator. NOT IMPLEMENTED.
  App&             operator=           (const App&);

  //! Copy constructor. NOT IMPLEMENTED.
                   App                 (const App&);

  License          license             () const;

protected:

                   App                 (int argc,
                                        char** argv,
                                        const CommandLine& cmdLine,
                                        License license = UNKNOWN);

  void             setParseCommandLine (bool setting);

  virtual void     showSplash          () const;

  /*!
    You only need to handle app specific exceptions in this functions. This
    function is called by run(), and default handlers for the base exceptions
    are defined there.

    Ofcourse you are allowed to catch every exception you want but do it only
    if you have some special handling code for them.

    Since all command line parsing has already been done (by run()), we
    know that if we get here we only have to test if the command line arguments
    contain useful stuff. So typically you let exec() test the command line's
    arguments, set up your application and then do the real stuff your class
    was designed to do.
  */
  virtual int      exec                () = 0;

  void             showUnhandledExceptionError(const std::string& message = "");

  void             showProgrammingError(const std::string& message = "");

  int              argc                () const;

  char**           argv                () const;

  std::string      licenseDescription  () const;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  virtual          ~App                ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  int              run                 ();

  void             addArgument         (CommandLineArgument* arg,
                                        bool isDefaultArgument = false);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  const std::string& name              () const;

  const std::string& version           () const;

  const CommandLine& commandLine       () const;

  const PathName   applicationDirectory() const;


  //! Shows an informative message.
  /*!
    \param     msg Message.
    \sa        showWarning(const std::string& msg),
               showError(const std::string& msg)

    A newline is appended to msg.
  */
  virtual void     showInfo            (const std::string& msg) const = 0;

  //! Shows a warning message.
  /*!
    \param     msg Message.
    \sa        showInfo(const std::string& msg),
               showError(const std::string& msg)

    A newline is appended to msg.
  */
  virtual void     showWarning         (const std::string& msg) const = 0;

  //! Shows an error message.
  /*!
    \param     msg Message.
    \sa        showInfo(const std::string& msg),
               showWarning(const std::string& msg)

    A newline is appended to msg.
  */
  virtual void     showError           (const std::string& msg) const = 0;

  virtual void     showError           (Exception::const_iterator begin,
                                        Exception::const_iterator end) const = 0;

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



} // namespace com

#endif
