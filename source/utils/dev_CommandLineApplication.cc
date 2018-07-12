#ifndef INCLUDED_DEV_COMMANDLINEAPPLICATION
#include "dev_CommandLineApplication.h"
#define INCLUDED_DEV_COMMANDLINEAPPLICATION
#endif

// Library headers.
#ifndef INCLUDED_IOSTREAM
#include <iostream>
#define INCLUDED_IOSTREAM
#endif

#ifndef INCLUDED_BOOST_CONVENIENCE
#include <boost/filesystem/convenience.hpp>
#define INCLUDED_BOOST_CONVENIENCE
#endif

#ifndef INCLUDED_BOOST_FILESYSTEM_PATH
#include <boost/filesystem/path.hpp>
#define INCLUDED_BOOST_FILESYSTEM_PATH
#endif

#ifndef INCLUDED_BOOST_FORMAT
#include <boost/format.hpp>
#define INCLUDED_BOOST_FORMAT
#endif

// Project headers.

// Module headers.



/*!
  \file
  This file contains the implementation of the CommandLineApplication class.
*/



namespace dev {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC COMMANDLINEAPPLICATION MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF COMMANDLINEAPPLICATION MEMBERS
//------------------------------------------------------------------------------

//! Constructor.
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .

  Options to show a help message (--help) and version info (--version) are
  added by default.
*/
CommandLineApplication::CommandLineApplication(
         unsigned short argc,
         char** argv,
         unsigned short major,
         unsigned short minor,
         unsigned short patch,
         std::string const& buildStage)

  : _argc(argc),
    _argv(argv),
    _parser(argc, argv),
    _genericOptions("options"),
    _major(major),
    _minor(minor),
    _patch(patch),
    _buildStage(buildStage)

{
  namespace bf = boost::filesystem;

  // Command name is argv[0] without the path and extension.
  bf::path argv0Path(argv[0]);
  _commandName = argv0Path.stem().string();


  _genericOptions.add_options()
         ("help", "Produce help message.")
         ("version", "Show version.")
         ;
}



//! Destructor.
/*!
*/
CommandLineApplication::~CommandLineApplication()
{
}



boost::program_options::command_line_parser& CommandLineApplication::commandLineParser()
{
  return _parser;
}



boost::program_options::options_description&
CommandLineApplication::genericOptions()
{
  return _genericOptions;
}



boost::program_options::options_description&
CommandLineApplication::hiddenOptions()
{
  return _hiddenOptions;
}



// boost::program_options::positional_options_description
// CommandLineApplication::positionalOptions()
// {
//   return _positionalOptions;
// }



void CommandLineApplication::addPositionalOption(
         std::string const& name,
         short maxCount,
         std::string description)
{
  assert(maxCount == -1 || maxCount > 0);
  assert(_positionalInfo.empty() || _positionalInfo.back().get<0>() > 0);

  _positionalOptions.add(name.c_str(), maxCount);
  _positionalInfo.push_back(boost::make_tuple(maxCount, description));
}



boost::program_options::variables_map
CommandLineApplication::programOptions() const
{
  return _variablesMap;
}



unsigned short CommandLineApplication::argc() const
{
  return _argc;
}



char** CommandLineApplication::argv() const
{
  return _argv;
}



std::string const& CommandLineApplication::commandName() const
{
  return _commandName;
}



void CommandLineApplication::usage(
         std::ostream& stream) const
{
  stream << commandName() << " options";

  unsigned short position = 0;

  for(unsigned short i = 0; i < _positionalInfo.size(); ++i) {
    if(_positionalInfo[i].get<0>() == -1) {
      position += 1;
    }
    else {
      position += _positionalInfo[i].get<0>();
    }

    stream << " " << _positionalOptions.name_for_position(position - 1);

    // stream << " " << _positionalInfo[i].get<1>();

    if(_positionalInfo[i].get<0>() == -1) {
      stream << " ...";
    }
  }

  stream << "\n\n"
         << _genericOptions << std::endl;

  position = 0;

  for(unsigned short i = 0; i < _positionalInfo.size(); ++i) {
    if(_positionalInfo[i].get<0>() == -1) {
      position += 1;
    }
    else {
      position += _positionalInfo[i].get<0>();
    }

    stream << _positionalOptions.name_for_position(position - 1) << ": "
           << _positionalInfo[i].get<1>() << "\n";
  }
}



//! Parses the command line.
/*!
  \param     .
  \return    Result code, either EXIT_FAILURE or EXIT_SUCCESS. In case the user
             asked for the usage information (--help) or version information
             (--version), than EXIT_SUCCESS is returned.
  \exception .
  \warning   .
  \sa        .
  \todo      Add license to message printed with --version.

  If the user asks for usage information (--help) or version information
  (--version), it is handled here.
  You want to stop executing the command in that case.

  If the help or version options are not given, you can do your stuff.

  No exceptions related to program option parsing leave this function. They
  are all handled here.
*/
int CommandLineApplication::parseCommandLine()
{
  namespace po = boost::program_options;

  int result = EXIT_FAILURE;

  po::options_description commandlineOptions;
  commandlineOptions.add(_genericOptions).add(_hiddenOptions);

  try {
    po::store(_parser
           .options(commandlineOptions)
           .positional(_positionalOptions).run(), _variablesMap);
    po::notify(_variablesMap);
    result = EXIT_SUCCESS;
  }
  catch(boost::program_options::unknown_option const& exception) {
    std::cerr << exception.what() << "\n";
    usage(std::cerr);
  }
  catch(boost::program_options::error const& exception) {
    std::cerr << "error: " << exception.what() << "\n";
    std::cerr << "use -h or --help for usage information\n";
  }

  if(programOptions().count("help")) {
    usage(std::cout);
  }
  else if(programOptions().count("version")) {
    std::cout << commandName() << ' ' << version() << std::endl;
  }

  return result;
}



void CommandLineApplication::setVersion(
         unsigned short major,
         unsigned short minor,
         unsigned short patch,
         std::string const& buildStage)
{
  _major = major;
  _minor = minor;
  _patch = patch;

  setBuildStage(buildStage);
}



//! Returns the version as a string.
/*!
  \return    String

  If a build stage is set, it is appended to the version information. Examples:
  - 0.0.1
  - 5.1.2-beta1
*/
std::string CommandLineApplication::version() const
{
  std::string result = (boost::format("%1%.%2%.%3%")
         % _major % _minor % _patch).str();

  if(!_buildStage.empty()) {
    result += "-" + _buildStage;
  }

  return result;
}



//! Set the build stage to \a stage.
/*!
  \param     stage Build stage.

  Build stage typically is something like alpha, beta, rc or similar. Possibly
  with a number attached to it, e.g. beta2. But you are free to use whatever
  you like. You probably do not want to set a build stage for release
  software.
*/
void CommandLineApplication::setBuildStage(
         std::string const& stage)
{
  _buildStage = stage;
}



std::string const& CommandLineApplication::buildStage() const
{
  return _buildStage;
}



void CommandLineApplication::setLicense(
         std::string const& license)
{
  _license = license;
}



void CommandLineApplication::applyGplLicense(
         std::string const& copyrightHolder)
{
  setLicense(
    "Copyright (C) 2008-2018 " + copyrightHolder + "\n"
    "\n"
    "This program is free software; you can redistribute it and/or\n"
    "modify it under the terms of the GNU General Public License\n"
    "as published by the Free Software Foundation; either version 3\n"
    "of the License, or (at your option) any later version.\n"
    "\n"
    "This program is distributed in the hope that it will be useful,\n"
    "but WITHOUT ANY WARRANTY; without even the implied warranty of\n"
    "MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n"
    "GNU General Public License for more details.\n"
    "\n"
    "You should have received a copy of the GNU General Public License\n"
    "along with this program; if not, write to the Free Software\n"
    "Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.\n");
}



std::string const& CommandLineApplication::license() const
{
  return _license;
}



void CommandLineApplication::showMessage(
         std::ostream& stream,
         std::string const& prefix,
         std::string const& message)
{
  stream << prefix << ": " << message;

  if(!message.empty() && *message.rbegin() != '\n') {
    stream << '\n';
  }
}



void CommandLineApplication::showProgrammingError(
         std::string const& message)
{
  showMessage(std::cerr, "programming error", message);
}



void CommandLineApplication::showUnhandledException(
         std::string const& message)
{
  showMessage(std::cerr, "unhandled exception", message);
}



void CommandLineApplication::showInfo(
         std::string const& message)
{
  std::cout << message;

  if(!message.empty() && *message.rbegin() != '\n') {
    std::cout << '\n';
  }
}



void CommandLineApplication::showWarning(
         std::string const& message)
{
  std::cout << message;

  if(!message.empty() && *message.rbegin() != '\n') {
    std::cout << '\n';
  }
}



void CommandLineApplication::showError(
         std::string const& message)
{
  std::cerr << message;

  if(!message.empty() && *message.rbegin() != '\n') {
    std::cerr << '\n';
  }
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace dev

