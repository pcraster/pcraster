#include "dev_CommandLineApplication.h"

#include <cassert>
#include <filesystem>
#include <format>
#include <iostream>




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
    _major(major),
    _minor(minor),
    _patch(patch),
    _buildStage(buildStage)

{
  namespace fs = std::filesystem;

  // Command name is argv[0] without the path and extension.
  fs::path const argv0Path(argv[0]);
  _commandName = argv0Path.stem().string();

  _genericOptions = "options:" % (
    clipp::option("--help").doc("Produce help message."),
    clipp::option("--version").doc("Show version.")
  );
}



//! Destructor.
/*!
*/
CommandLineApplication::~CommandLineApplication()
{
}



clipp::group& CommandLineApplication::commandLineParser()
{
  return _parser;
}



clipp::group&
CommandLineApplication::genericOptions()
{
  return _genericOptions;
}



clipp::group&
CommandLineApplication::hiddenOptions()
{
  return _hiddenOptions;
}



clipp::group&
CommandLineApplication::positionalOptions()
{
  return _positionalOptions;
}



void CommandLineApplication::addPositionalOption(
         std::string const&  /*name*/,
         short maxCount,
         const std::string& description)
{
  assert(maxCount == -1 || maxCount > 0);
  assert(_positionalInfo.empty() || std::get<0>(_positionalInfo.back()) > 0);

  _positionalInfo.push_back(std::make_tuple(maxCount, description));
}


variables_map
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
  auto fmt = clipp::doc_formatting{}
         .line_spacing(0)
         .indent_size(2)
         .paragraph_spacing(1)
         .first_column(0)
         .doc_column(24)
         .last_column(80);

  stream << commandName() << " options " << clipp::usage_lines(_positionalOptions, fmt);

  stream << "\n\n"
         << clipp::documentation(_genericOptions, fmt).str()
         << "\n\n"
         << clipp::documentation(_positionalOptions, fmt).str()
         << "\n";
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
//   namespace po = boost::program_options;

  int const result = EXIT_FAILURE;

//   po::options_description commandlineOptions;
//   commandlineOptions.add(_genericOptions).add(_hiddenOptions);

//   try {
//     po::store(_parser
//            .options(commandlineOptions)
//            .positional(_positionalOptions).run(), _variablesMap);
//     po::notify(_variablesMap);
//     result = EXIT_SUCCESS;
//   }
//   catch(boost::program_options::unknown_option const& exception) {
//     std::cerr << exception.what() << "\n";
//     usage(std::cerr);
//   }
//   catch(boost::program_options::error const& exception) {
//     std::cerr << "error: " << exception.what() << "\n";
//     std::cerr << "use -h or --help for usage information\n";
//   }
//
//   if(programOptions().count("help")) {
//     usage(std::cout);
//   }
//   else if(programOptions().count("version")) {
//     std::cout << commandName() << ' ' << version() << std::endl;
//   }

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
  std::string result = std::format("{0}.{1}.{2}",
         _major, _minor, _patch);

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
    // "Copyright (C) 2008-20xy " + copyrightHolder + "\n"
#include "current_dev_year.inc"
    "" + copyrightHolder + "\n"
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
         std::string const& message) const
{
  std::cout << message;

  if(!message.empty() && *message.rbegin() != '\n') {
    std::cout << '\n';
  }
}



void CommandLineApplication::showWarning(
         std::string const& message) const
{
  std::cout << message;

  if(!message.empty() && *message.rbegin() != '\n') {
    std::cout << '\n';
  }
}



void CommandLineApplication::showError(
         std::string const& message) const
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

