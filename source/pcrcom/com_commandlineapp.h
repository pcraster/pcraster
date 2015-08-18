#ifndef INCLUDED_COM_COMMANDLINEAPP
#define INCLUDED_COM_COMMANDLINEAPP



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.
#ifndef INCLUDED_COM_APP
#include "com_app.h"
#define INCLUDED_COM_APP
#endif

// Module headers.



namespace com {
  // CommandLineApp declarations.
}



namespace com {



//! The CommandLineApp class is a base class for command line applications.
/*!
  Command line applications are run from the command line and have no graphical
  user interface.

  Arguments added to the list of command line arguments:

  <ul>
    <li>--quiet: prevents info messages to be printed</li>
  </ul>

  Example:
  \code
  class Foo: public com::CommandLineApp
  {
    com::PositionalValue<std::string> d_inputFileArg;
    com::PositionalValue<std::string> d_outputFileArg;

    Foo(int argc, char** argv)
      : CommandLineApp(argc, argv, CommandLine("foo", __DATE__, "foo")),
        d_inputFileArg ("inputMap" , "file name of input map" ,true),
        d_outputFileArg("outputMap", "file name of output map",true)
    {
      // Set the app's arguments.
      addArgument(&d_inputFileArg);
      addArgument(&d_outputFileArg);

      // Create a splash.
      createDefaultSplash();
    }

    int exec()
    {
      // Test the command line arguments (they have already been parsed).
      // Do the real works: what was Foo meant to do...
      ...
    }
  };

  int main(int argc,
           char *argv[])
  {
    Foo f(argc,argv);
    return f.run();
  }
  \endcode
*/
class CommandLineApp: public App
{

private:

  /*
  mutable size_t   d_nrMessagesOnCout;

  mutable size_t   d_nrMessagesOnCerr;
  */

  //! Command line argument for toggling splash print mode.
  Option           d_quietArg;

  //! Developer text.
  std::string      d_developer;

  //! Splash text.
  std::string      d_splash;

  //! Assignment operator. NOT IMPLEMENTED.
  CommandLineApp&  operator=           (const CommandLineApp&);

  //! Copy constructor. NOT IMPLEMENTED.
                   CommandLineApp      (const CommandLineApp&);

  void             showSplash          () const;

protected:

                   CommandLineApp      (int argc,
                                        char** argv,
                                        const CommandLine& cmdLine,
                                        License license = UNKNOWN);

  bool             quiet               () const;

  void             createDefaultSplash ();

  void             setDeveloper        (const std::string& developer);

  void             showInfo            (const std::string& msg) const;

  void             showWarning         (const std::string& msg) const;

  void             showError           (const std::string& msg) const;

  void             showError           (Exception::const_iterator begin,
                                        Exception::const_iterator end) const;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  virtual          ~CommandLineApp     ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

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
