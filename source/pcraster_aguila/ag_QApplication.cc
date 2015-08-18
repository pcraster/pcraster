#include "ag_QApplication.h"

// External headers.
#include <stdexcept>

// Project headers.
#include "dal_Exception.h"

// Module headers.
#include "com_exception.h"
#include "qt_AppWindow.h"



/*!
  \file
  This file contains the implementation of the QApplication class.
*/



namespace ag {

// Code that is private to this module.
namespace detail {

} // namespace detail



//------------------------------------------------------------------------------
// DEFINITION OF STATIC QAPPLICATION MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF QAPPLICATION MEMBERS
//------------------------------------------------------------------------------

QApplication::QApplication(
         int& argc,
         char** argv)

  : ::QApplication(argc, argv)

{
}



QApplication::~QApplication()
{
}



bool QApplication::notify(
         QObject* receiver,
         QEvent* event)
{
  try {
    return ::QApplication::notify(receiver, event);
  }
  catch(com::Exception const& exception) {
    qt::AppWindow::showError(_name, exception.messages());
  }
  catch(dal::Exception const& exception) {
    qt::AppWindow::showError(_name, exception.message());
  }
  catch(std::bad_alloc const&) {
    // Memory issues are handled in the new handler set above. See qt_GuiApp.cc
    // QApplication::setOverrideCursor(Qt::arrowCursor);
  }
  catch(std::logic_error const& exception) {
    qt::AppWindow::showError(_name,
         std::string("programming error\n") + exception.what());
  }
  catch(std::exception const& exception) {
    qt::AppWindow::showError(_name, exception.what());
  }
  catch(...) {
    qt::AppWindow::showError(_name, "unhandled exception");
  }

  exit(1);

  return false;
}



void QApplication::setName(
         std::string const& name)
{
  _name = name;
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace ag

