#ifndef INCLUDED_COM_COMMANDLINE
#define INCLUDED_COM_COMMANDLINE



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
#ifndef INCLUDED_COM_COMMANDLINEARGUMENTS
#include "com_commandlinearguments.h"
#define INCLUDED_COM_COMMANDLINEARGUMENTS
#endif



namespace com {
  // CommandLine declarations.
  class CommandLineArgument;
}



namespace com {



//! The CommandLine class is for encapsulating command lines.
/*!
  A command line is the string an application is called/started with. It
  consists of the application's name and zero or more command line arguments.
*/
class PCR_DLL_CLASS CommandLine
{

  friend class CommandLineTest;

private:

  //! Command name.
  const std::string d_name;

  //! Command version.
  const std::string d_version;

  //! Command name as used on the command line.
  const std::string d_command;

  #if _MSC_VER // == 1400
    #error // check _MSC_VER
    #pragma warning(disable:4251)
  #endif
  //! Command line arguments.
  CommandLineArguments d_arguments;
  #if _MSC_VER // == 1400
    #pragma warning(default:4251)
  #endif

  //! Assignment operator. NOT IMPLEMENTED.
  CommandLine&     operator=           (const CommandLine&);

  void             printSynopsis       (std::ostream& s,
                                        size_t offset,
                                        size_t width) const;

  void             printDescription    (std::ostream& s,
                                        size_t offset,
                                        size_t width) const;

  void             clear               ();

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   CommandLine         (const std::string& name,
                                        const std::string& version,
                                        const std::string& argv0);

/*
                   CommandLine         (const std::string& name,
                                        const std::string& version,
                                        CommandLineArgument* arg1...);
*/

                   CommandLine         (const CommandLine& aCmdLine);

  /* virtual */    ~CommandLine        ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

/*
  void             setArguments        (CommandLineArgument* arg1...);
*/

  void             addArgument         (CommandLineArgument* arg,
                                        bool isDefaultArgument = false);

  void             parse               (size_t argc,
                                        const char* const* argv);

  void             check               () const;

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  const std::string& name              () const;

  const std::string& version           () const;

  const std::string& command           () const;

  void             printUsage          (std::ostream& s,
                                        size_t offset = 0,
                                        size_t width = 80) const;

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
