#ifndef INCLUDED_DEV_COMMANDLINEAPPLICATION
#define INCLUDED_DEV_COMMANDLINEAPPLICATION



// Library headers.
#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif

#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
#endif

#ifndef INCLUDED_BOOST_NONCOPYABLE
#include <boost/noncopyable.hpp>
#define INCLUDED_BOOST_NONCOPYABLE
#endif

#ifndef INCLUDED_BOOST_PROGRAM_OPTIONS
#include <boost/program_options.hpp>
#define INCLUDED_BOOST_PROGRAM_OPTIONS
#endif

#ifndef INCLUDED_BOOST_TUPLE_TUPLE
#include <boost/tuple/tuple.hpp>
#define INCLUDED_BOOST_TUPLE_TUPLE
#endif

// Project headers.

// Module headers.



namespace dev {
  // CommandLineApplication declarations.
}



namespace dev {

//! Base class for command line application.
/*!
  This class contains some goodies which come in handy for command line
  applications, most importantly support for parsing command line arguments.

  \sa        .
  \todo      Make run() pure virtual. The only reason it is no so right now
             is because the unit test requires a CommandLineApplication object.
             Maybe define a symbol during unit test app build which triggers
             the include of a non pure virtual run().
*/
class CommandLineApplication: private boost::noncopyable
{

private:

  unsigned short   _argc;

  char**           _argv;

  boost::program_options::command_line_parser _parser;

  std::string      _commandName;

  boost::program_options::options_description _genericOptions;

  boost::program_options::options_description _hiddenOptions;

  boost::program_options::positional_options_description _positionalOptions;

  std::vector< boost::tuple<short, std::string> > _positionalInfo;

  boost::program_options::variables_map _variablesMap;

  //! Major version number.
  unsigned short   _major;

  //! Minor version number.
  unsigned short   _minor;

  //! Patch version number.
  unsigned short   _patch;

  //! Build stage.
  std::string      _buildStage;

  //! License description.
  std::string      _license;

  void             showMessage         (std::ostream& stream,
                                        std::string const& prefix,
                                        std::string const& message);

protected:

  boost::program_options::command_line_parser& commandLineParser();

  boost::program_options::options_description& genericOptions();

  boost::program_options::options_description& hiddenOptions();

  // boost::program_options::positional_options_description positionalOptions();

  void             addPositionalOption (std::string const& name,
                                        short maxCount,
                                        std::string description);

  boost::program_options::variables_map programOptions() const;

  void             usage               (std::ostream& stream) const;

  int              parseCommandLine    ();

  void             setVersion          (unsigned short major,
                                        unsigned short minor,
                                        unsigned short patch,
                                        std::string const& buildStage="");

  void             setBuildStage       (std::string const& stage);

  void             setLicense          (std::string const& license);

  void             applyGplLicense     (std::string const& copyrightHolder);

  void             showProgrammingError(std::string const& message);

  void             showUnhandledException(
                                        std::string const& message="");

  virtual void     showInfo            (std::string const& message);

  virtual void     showWarning         (std::string const& message);

  virtual void     showError           (std::string const& message);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   CommandLineApplication(unsigned short argc,
                                       char** argv,
                                       unsigned short major=0,
                                       unsigned short minor=0,
                                       unsigned short patch=0,
                                       std::string const& buildStage="");

  virtual          ~CommandLineApplication();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

// #ifdef DEBUG_BUILD
  // Needed for CommandLineApplicationTest.
  virtual int      run                 () { return 0; };
// #else
//   virtual int      run                 ()=0;
// #endif

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  unsigned short   argc                () const;

  char**           argv                () const;

  std::string const& commandName       () const;

  std::string      version             () const;

  std::string const& buildStage        () const;

  std::string const& license           () const;

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

} // namespace dev

#endif
