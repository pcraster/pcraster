#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_COM_COMMANDLINEAPP
#include "com_commandlineapp.h"
#define INCLUDED_COM_COMMANDLINEAPP
#endif

// Library headers.
#ifndef INCLUDED_IOSTREAM
#include <iostream>
#define INCLUDED_IOSTREAM
#endif

// PCRaster library headers.

// Module headers.



/*!
  \file
  This file contains the implementation of the CommandLineApp class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC COMMANDLINEAPP MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF COMMANDLINEAPP MEMBERS
//------------------------------------------------------------------------------

//! ctor
com::CommandLineApp::CommandLineApp(int argc, char** argv,
                   const CommandLine& cmdLine, License license)

  : App(argc, argv, cmdLine, license),
    // d_nrMessagesOnCout(0), d_nrMessagesOnCerr(0),
    d_quietArg("quiet", "do not print info messages", false)

{
  addArgument(&d_quietArg, true);
}



//! dtor
com::CommandLineApp::~CommandLineApp()
{
}



//! Returns whether the app should run in quiet mode or not.
/*!
  \return    true or false.
  \warning   Make sure you check this mode at all relevant locations in your
             app code.
*/
bool com::CommandLineApp::quiet() const
{
  return d_quietArg.isParsed();
}



/*!
  \warning   Nothing happens if the app runs in quiet mode.
*/
void com::CommandLineApp::showInfo(const std::string& msg) const
{
  if(!quiet()) {
    /*
    if(d_nrMessagesOnCerr > 0 || d_nrMessagesOnCout > 0) {
      std::cout << std::endl;
    }
    std::cout << msg << std::flush;
    ++d_nrMessagesOnCout;
    */
    std::cout << msg << std::endl;
  }
}



void com::CommandLineApp::showWarning(const std::string& msg) const
{
  /*
  if(d_nrMessagesOnCerr > 0 || d_nrMessagesOnCout > 0) {
    std::cerr << std::endl;
  }
  std::cerr << "warning: " << msg << std::flush;
  ++d_nrMessagesOnCerr;
  */
  std::cerr << "warning: " << msg << std::endl;
}



void com::CommandLineApp::showError(const std::string& msg) const
{
  /*
  if(d_nrMessagesOnCerr > 0 || d_nrMessagesOnCout > 0) {
    std::cerr << std::endl;
  }
  std::cerr << "error: " << msg << std::flush;
  ++d_nrMessagesOnCerr;
  */
  std::cerr << "error: " << msg << std::endl;
}



void com::CommandLineApp::showError(Exception::const_iterator begin,
         Exception::const_iterator end) const
{
  for(Exception::const_iterator it = begin; it != end; ++it) {
    showError(*it);
  }
}



//! Shows the splash message.
/*!
  \warning   Nothing happens if the splash message is empty.
  \sa        createDefaultSplash(), setSplash(const std::string&), showInfo()
*/
void com::CommandLineApp::showSplash() const
{
  if(!d_splash.empty()) {
    showInfo(d_splash);
  }
}



//! Creates a default splash message.
/*!
  \sa        setSplash(const std::string&), showSplash()

  A splash message is a message the application starts with. The default
  message create here contains the name and the version of the application.
*/
void com::CommandLineApp::createDefaultSplash()
{
  d_splash = name() + ' ' + version();

  if(!d_developer.empty()) {
    d_splash += "\n" + d_developer;
  }
}



// Sets the splash message to \a splash.
/*
  \param     splash New splash message.
  \sa        createDefaultSplash(), showSplash()
*/
/*
void com::CommandLineApp::setSplash(const std::string& splash)
{
  d_splash = splash;
}
*/



//! Sets the developer text.
/*!
  \param     developer Text with name of developer.
  \sa        createDefaultSplash()

  This string is used in the default splash text.
*/
void com::CommandLineApp::setDeveloper(const std::string& developer)
{
  d_developer = developer;
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



