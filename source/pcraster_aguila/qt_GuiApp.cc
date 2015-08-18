#include "qt_GuiApp.h"

// std
#include <new>
#include <string>
#include <sstream>
#include <boost/filesystem/exception.hpp>
#include <boost/filesystem/fstream.hpp>
#include <boost/filesystem/operations.hpp>
#include <boost/format.hpp>

/*
#ifdef WIN32
  #ifndef INCLUDED_QWINDOWSSTYLE
#include <QWindowsStyle>
  #define INCLUDED_QWINDOWSSTYLE
  #endif
#elif __linux__
  #ifndef INCLUDED_QMOTIFSTYLE
#include <QMotifStyle>
  #define INCLUDED_QMOTIFSTYLE
  #endif
#else
  #error unknown target platform
#endif
*/

// pcr
#include "dal_Exception.h"
#include "dal_FilesystemUtils.h"
#include "qt_AppWindow.h"
#include "qt_New.h"



/*!
  \file
  This file contains the implementation of the GuiApp class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC CLASS MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF CLASS MEMBERS 
//------------------------------------------------------------------------------

//! Constructs a GuiApp object.
/*!
  \param   argc Argument count.
  \param   argv Argument vector.
  \param   cmdLine Application's command line configuration.
  \warning Don't throw exceptions while constructing the application. The
           constructor is only meant for configuration of the app. You get
           the chance to create and setup your application by overriding
           the setup() function.

  If the last application window is closed, than QApplication::quit() is called.
*/
qt::GuiApp::GuiApp(
         int& argc,
         char** argv,
         // const com::CommandLine& cmdLine,
         // com::License license,
         ApplicationRole role)

  : QObject(0),
    dev::QtClient<ag::QApplication>(argc, argv),
    dev::CommandLineApplication(argc, argv)
    // com::App(argc, argv, cmdLine, license)

{
  assert(dev::QtClient<ag::QApplication>::isInitialized());
  dev::QtClient<ag::QApplication>::application().setName(commandName());

  // Configure application.
  // Set the new handler to call when the app goes out of memory.
  std::set_new_handler(qt::outOfMemoryHandler);

  // This sets the back ground of widgets of the named class, to white.
  // QPalette palette;
  // palette.setColor(QPalette::Window, Qt::white);
  // QApplication::setPalette(palette, "QwtPlotCanvas");
  // QApplication::setPalette(palette, "ag::Map2DView");

  if(role == StandAlone) {
    connect(&application(), SIGNAL(lastWindowClosed()),
         &application(), SLOT(quit()));
  }
}



//! Destructs the GuiApp object.
/*!
*/
qt::GuiApp::~GuiApp()
{
  try {
    if(!d_lockFilename.empty()) {
      deleteLockFile();
    }
  }
  catch(com::Exception &exception) {
    showError(exception.messages());
  }
}



// int qt::GuiApp::run()
// {
//   setup();
//   return d_app->exec();
// }



//! Starts the application and returns the return code.
/*!
  \return  Return status of the application.

  This function calls the subclass' setup() function and after that it starts
  the event loop of the qt-application.

  This function returns 0 if everything went well. If an error occured, than
  1 is returned.

  No exception leaves this function.
*/
int qt::GuiApp::run()
{
  int status = 1;

  try {
    setup();
    status = application().exec();
  }
  catch(com::Exception const& exception) {
    showError(exception.messages());
  }
  catch(dal::Exception const& exception) {
    showError(exception.message());
  }
  catch(std::exception const& exception) {
    showError(exception.what());
  }
  catch(...) {
    showUnhandledException();
  }

  return status;
}



//! Quits the application.
/*!
*/
void qt::GuiApp::quit()
{
  application().quit();
}



//! Creates a lock file.
/*!
  \param     pathName File name of the lock file.
  \sa        deleteLockFile()

  A lock file is a file which is created once and is deleted when the
  application exits (by deleteLockFile()). This can be useful if other
  processes need to keep track of the existence of this application.

  A lock file is created only once.

  This is an optional facility.
*/
void qt::GuiApp::createLockFile(std::string const& filename)
{
  assert(d_lockFilename.empty());

  namespace bfs = boost::filesystem;

  d_lockFilename = bfs::path(filename);

  if(!bfs::exists(d_lockFilename)) {
    bfs::ofstream file(d_lockFilename);
    assert(bfs::exists(d_lockFilename));
  }
  else {
    bfs::file_status status(bfs::status(d_lockFilename));
    assert(bfs::status_known(status));

    if(!bfs::is_regular_file(status)) {
      throw com::FileError(d_lockFilename.string(), std::string(
                   "existing lock file is not a regular file"));
    }
    else if(!dal::isWritable(d_lockFilename)) {
      throw com::FileError(d_lockFilename.string(), std::string(
                   "existing lock file is not writable and cannot be deleted"));
    }
  }
}



//! Deletes the lock file associated with this application.
/*!
  \sa        createLockFile(const com::PathName&)
*/
void qt::GuiApp::deleteLockFile()
{
  if(!d_lockFilename.empty() && boost::filesystem::exists(d_lockFilename)) {
    try {
      boost::filesystem::remove(d_lockFilename);
    }
    catch(boost::filesystem::filesystem_error const&) {
      showWarning((boost::format("Lock file %1% cannot be deleted")
         % d_lockFilename.string()).str());
    }
  }
}



void qt::GuiApp::showInfo(const std::string& message) const
{
  qt::AppWindow::showInfo(commandName(), message);
}



void qt::GuiApp::showWarning(const std::string& message) const
{
  qt::AppWindow::showWarning(commandName(), message);
}



void qt::GuiApp::showError(const std::string& message) const
{
  qt::AppWindow::showError(commandName(), message);
}



void qt::GuiApp::showError(com::Exception::const_iterator begin,
         com::Exception::const_iterator end) const
{
  assert(begin != end);

  std::string message = *begin;

  for(com::Exception::const_iterator it = ++begin; it != end; ++it) {
    message += ('\n' + *it);
  }

  showError(message);
}



//------------------------------------------------------------------------------
// DOCUMENTATION OF ENUMERATIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DOCUMENTATION OF INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DOCUMENTATION OF PURE VIRTUAL FUNCTIONS
//------------------------------------------------------------------------------

/*!
  \fn        void qt::GuiApp::setup()
  \brief     Creates / sets up the application.
  \sa        run()

  Override this to setup your application. Exception are caught by the run()
  function.
*/

