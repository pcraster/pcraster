#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_COM_APP
#include "com_app.h"
#define INCLUDED_COM_APP
#endif

// Library headers.
#ifndef INCLUDED_CSTDARG
#include <cstdarg>
#define INCLUDED_CSTDARG
#endif

#ifndef INCLUDED_STDEXCEPT
#include <stdexcept>
#define INCLUDED_STDEXCEPT
#endif

#ifndef INCLUDED_SSTREAM
#include <sstream>
#define INCLUDED_SSTREAM
#endif

// PCRaster library headers.
#ifndef INCLUDED_DAL_EXCEPTION
#include "dal_Exception.h"
#define INCLUDED_DAL_EXCEPTION
#endif

// Module headers.



#ifdef DEBUG_DEVELOP
com::DebugStream dbs;
#endif



/*!
  \file
  This file contains the implementation of the App class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC APP MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF APP MEMBERS
//------------------------------------------------------------------------------

//! Constructor.
/*!
  \param     argc Argument count.
  \param     argv Argument vector.
  \param     cmdLine Application's command line.
  \param     license License.

  Default license is UNKNOWN. No --license option will be available in the
  command usage. If another license is used information will be printed for
  that specific license using the information returned by licenseDescription().
*/
com::App::App(int argc, char** argv, const CommandLine& cmdLine,
         License license)

  : d_argc(argc), d_argv(argv),
    d_helpArg('h', "help", "print usage information", false),
    d_versionArg('v', "version", "print version information", false),
    d_licenseArg("license", "print license information", false),
#ifdef DEBUG_DEVELOP
    d_debugStreamArg("debug", "debug.out",
         "file name for debug output", false),
#endif
    d_cmdLine(cmdLine), d_parseCommandLine(true),
    d_license(license)

{
/*
  // http://gcc.gnu.org/onlinedocs/libstdc++/19_diagnostics/howto.html
  // wordt default in GCC 3.4
  std::set_terminate (__gnu_cxx::__verbose_terminate_handler);
 */

  addArgument(&d_helpArg, true);
  addArgument(&d_versionArg, true);
#ifdef DEBUG_DEVELOP
  addArgument(&d_debugStreamArg, true);
#endif

  if(license == GNU) {
    addArgument(&d_licenseArg, true);
  }
}



//! Destructor.
/*!
*/
com::App::~App()
{
}



void com::App::setParseCommandLine(
         bool setting)
{
  d_parseCommandLine = setting;
}



int com::App::argc() const
{
  return d_argc;
}



char** com::App::argv() const
{
  return d_argv;
}



void com::App::addArgument(CommandLineArgument* arg, bool isDefaultArgument)
{
  d_cmdLine.addArgument(arg, isDefaultArgument);
}



//! Shows the splash screen, if information is available.
/*!
  The default does nothing.
*/
void com::App::showSplash() const
{
}



//! Runs/starts the application.
/*!
  \return    Return value of exec() in case of success and EXIT_FAILURE
             otherwise.
  \todo      CW: return 0 or !0 is too limited for general use,
             it should return the code of exec(), whatever that is.

  No exceptions leave this function.

  This function:
  <ol>
    <li>parses the command line</li>
    <li>calls showSplash()</li>
    <li>calls exec() for the real works</li>
  </ol>
*/
int com::App::run()
{
  int status;

  try {

    // Parse the command line.
    if(d_parseCommandLine) {
      PRECOND(d_argc >= 1);
      d_cmdLine.parse(static_cast<size_t>(d_argc - 1), d_argv + 1);

      // See if our standard arguments are given.
      if(d_helpArg.isParsed()) {
        std::ostringstream stream;
        stream << d_cmdLine.name() << std::string(" ") << version()
          << std::string("\n\n");
        d_cmdLine.printUsage(stream);

        std::string usage(stream.str());
        POSTCOND(!usage.empty() && usage[usage.size() - 1] == '\n');
        showInfo(std::string(usage.begin(), usage.end() - 1));

        return EXIT_SUCCESS;
      }
      else if(d_versionArg.isParsed()) {
        std::ostringstream stream;
        stream << version();
        showInfo(stream.str());

        return EXIT_SUCCESS;
      }
      else if(d_licenseArg.isParsed()) {
        std::ostringstream stream;
        stream << licenseDescription();
        showInfo(stream.str());

        return EXIT_SUCCESS;
      }
      else {
#ifdef DEBUG_DEVELOP
        if(d_debugStreamArg.isParsed()) {
          PRECOND(!dbs.is_open());
          dbs.open(d_debugStreamArg.value().c_str());
          POSTCOND(dbs.is_open());
        }
#endif

        d_cmdLine.check();
      }
    }

    showSplash();
    status = exec();
  }
  catch(const CommandLineException& exception) {
    showError(exception.begin(), exception.end());

    if(d_argc == 1) {
      // In this case the user might not know anything about the command.
      std::ostringstream stream;
      d_cmdLine.printUsage(stream);
      showInfo(stream.str());
    }

    status = EXIT_FAILURE;
  }
  catch(Exception& exception) {
    showError(exception.begin(), exception.end());

    status = EXIT_FAILURE;
  }
  catch(dal::Exception& exception) {
    showError(exception.message());

    status = EXIT_FAILURE;
  }
  catch(std::bad_alloc& /* exception */) {
    // A memory handler might have been installed which runs first. In case
    // we end up here, all bets are off.
    showError("Not enough memory");
    // showError(exception.what());

    status = EXIT_FAILURE;
  }
  catch(std::logic_error &exception) {
    showProgrammingError(exception.what());

    status = EXIT_FAILURE;
  }
  catch(std::exception& exception) {
    showProgrammingError(exception.what());

    status = EXIT_FAILURE;
  }
#ifndef DEBUG_DEVELOP  // Makes debugging more convenient.
  catch (...) {
    showUnhandledExceptionError();

    status = EXIT_FAILURE;
  }
#endif

  return status;
}



//! Shows a message saying that an unhandled exception was caught.
/*!
  Just to make sure that all these kinds of messages look the same.
*/
void com::App::showUnhandledExceptionError(const std::string& message)
{
  if(message.empty()) {
    showError("Programming error: Unhandled exception");
  }
  else {
    std::ostringstream stream;
    stream << "Programming error: " << message;
    showError(stream.str());
  }
}



void com::App::showProgrammingError(const std::string& message)
{
  if(message.empty()) {
    showError("Programming error: Unhandled exception");
  }
  else {
    std::ostringstream stream;
    stream << "Programming error: " << message;
    showError(stream.str());
  }
}



//! Returns the name of the application.
/*!
  \return    Name.
  \sa        version()
*/
const std::string& com::App::name() const
{
  return d_cmdLine.name();
}



//! Return the version of the application.
/*!
  \return    Version.
  \sa        name()
*/
const std::string& com::App::version() const
{
  return d_cmdLine.version();
}



com::License com::App::license() const
{
  return d_license;
}



std::string com::App::licenseDescription() const
{
  PRECOND(license() == GNU);

  std::string description;

  // if(license() == GNU) {
    description =
    "This program is free software; you can redistribute it and/or modify\n" \
    "it under the terms of the GNU General Public License as published by\n" \
    "the Free Software Foundation; either version 2 of the License, or\n" \
    "(at your option) any later version.\n" \
    "\n" \
    "This program is distributed in the hope that it will be useful,\n" \
    "but WITHOUT ANY WARRANTY; without even the implied warranty of\n" \
    "MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n" \
    "GNU General Public License for more details.\n" \
    "\n" \
    "You should have received a copy of the GNU General Public License\n" \
    "along with this program; if not, write to the Free Software\n" \
    "Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA";
  // }

  return description;
}



//! Returns the command line of the application.
/*!
  \return    Command line.
*/
const com::CommandLine& com::App::commandLine() const
{
  return d_cmdLine;
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



