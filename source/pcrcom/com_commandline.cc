#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_COM_COMMANDLINE
#include "com_commandline.h"
#define INCLUDED_COM_COMMANDLINE
#endif

// Library headers.
#ifndef INCLUDED_IOSTREAM
#include <iostream>
#define INCLUDED_IOSTREAM
#endif

#ifndef INCLUDED_SSTREAM
#include <sstream>
#define INCLUDED_SSTREAM
#endif

#ifndef INCLUDED_BOOST_FILESYSTEM_PATH
#include <boost/filesystem/path.hpp>
#define INCLUDED_BOOST_FILESYSTEM_PATH
#endif

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_COM_COMMANDLINEARGUMENT
#include "com_commandlineargument.h"
#define INCLUDED_COM_COMMANDLINEARGUMENT
#endif

#ifndef INCLUDED_COM_STRLIB
#include "com_strlib.h"
#define INCLUDED_COM_STRLIB
#endif



/*!
  \file
  This file contains the implementation of the CommandLine class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC COMMANDLINE MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF COMMANDLINE MEMBERS
//------------------------------------------------------------------------------

//! Constructor.
/*!
  \param     name Command's name.
  \param     version Command's version.
  \param     argv0 Command name as used on the command line.
*/
com::CommandLine::CommandLine(const std::string& name,
                   const std::string& version, const std::string& argv0)

  : d_name(name), d_version(version), d_command(
    boost::filesystem::path(argv0).filename().string())

{
}



//! Copy constructor.
/*!
  \param     aCmdLine CommandLine object to copy from.
  \warning   Shallow copy of command line arguments.
*/
com::CommandLine::CommandLine(const CommandLine& aCmdLine)

  : d_name(aCmdLine.d_name), d_version(aCmdLine.d_version),
    d_command(aCmdLine.d_command), d_arguments(aCmdLine.d_arguments)

{
}



//! Destructor.
/*!
*/
com::CommandLine::~CommandLine()
{
}



//! Adds an argument to the command line.
/*!
  \param     arg Argument to add.
  \warning   arg must be !0.
  \sa        setArguments(CommandLineArgument*)

  This function should be called before the command line is
  parse(int, const char* const*) d.
*/
void com::CommandLine::addArgument(CommandLineArgument* arg,
         bool isDefaultArgument)
{
  d_arguments.add(arg, isDefaultArgument);
}



//! Parses the command line.
/*!
  \param     argc Argument count, number of tokens in \a argv.
  \param     argv Argument vector, without the first argument (program name).
  \exception CommandLineException In case of a syntax error.
  \warning   Make sure the command line knows about all your application's
             arguments.

  If this function succeeds than you know for sure that the syntax of the
  command line was in sync with all configured command line arguments and
  those arguments who need a value have been given a value. You only need to
  test these values.
*/
void com::CommandLine::parse(size_t argc, const char* const* argv)
{
  // Make a local copy of the argv argument because it will be edited during
  // the parse.
  char** localArgv = new char*[argc];
  for(size_t i = 0; i < argc; ++i) {
    localArgv[i] = new char[std::strlen(argv[i])+1];
    strcpy(localArgv[i], argv[i]);
  }

  try {
    d_arguments.parse(argc, localArgv);
  }
  catch(...) {

    for(size_t i = 0; i < argc; ++i) {
      delete[] localArgv[i];
    }
    delete[] localArgv;

    throw;
  }

  for(size_t i = 0; i < argc; ++i) {
    delete[] localArgv[i];
  }
  delete[] localArgv;
}



void com::CommandLine::check() const
{
  d_arguments.check();
}



//! Returns the name of the command.
/*!
  \return    Name.
  \sa        version(), command()
*/
const std::string& com::CommandLine::name() const
{
  return d_name;
}



//! Returns the name of the version.
/*!
  \return    Version.
  \sa        name(), command()
*/
const std::string& com::CommandLine::version() const
{
  return d_version;
}



//! Returns the name of the command as used on the command line.
/*!
  \return    Name.
  \sa        name(), version()
*/
const std::string& com::CommandLine::command() const
{
  return d_command;
}



void com::CommandLine::printUsage(std::ostream& stream,
                   size_t offset, size_t width) const
{
  printSynopsis(stream, offset + 4, width - 4);
  stream << std::string("\n");
  printDescription(stream, offset, width);
}



void com::CommandLine::printSynopsis(std::ostream& stream,
                   size_t offset, size_t width) const
{
  stream << std::string("Usage:\n\n");

  // Print all info in a string stream.
  std::ostringstream stringStream;
  stringStream << command();
  d_arguments.printSynopsis(stringStream);

  // Format synopsis string.
  std::string synopsis = format(stringStream.str(), offset, width);

  // Print formatted synopsis string.
  stream << synopsis << std::string("\n");
}



void com::CommandLine::printDescription(std::ostream& stream,
                   size_t offset, size_t width) const
{
  d_arguments.printDescription(stream, offset, width);
}



void com::CommandLine::clear()
{
  d_arguments.clear();
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



