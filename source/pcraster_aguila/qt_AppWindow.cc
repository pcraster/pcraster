#include "qt_AppWindow.h"
#include <sstream>

// Qt
#include <QApplication>
#include <QCloseEvent>
#include <QIcon>
#include <QMessageBox>
#include "qt_AppWindowProperties.h"



//------------------------------------------------------------------------------
// DEFINITION OF STATIC CLASS MEMBERS
//------------------------------------------------------------------------------

namespace qt {

ApplicationRole AppWindow::d_role = StandAlone;



void AppWindow::setApplicationRole(
         ApplicationRole role)
{
  d_role = role;
}



ApplicationRole AppWindow::applicationRole()
{
  return d_role;
}

} // namespace qt



//! Shows an informative message.
/*!
  \param     app Name of the application.
  \param     msg Message.

  Use this function if the message is not related to a certain parent widget.
*/
void qt::AppWindow::showInfo(const std::string& app, const std::string& msg)
{
  showInfo(0, app, msg);
}



//! Shows a warning message.
/*!
  \param     caption Caption
  \param     msg Message.

  Use this function if the message is not related to a certain parent widget.
*/
void qt::AppWindow::showWarning(const std::string& caption, const std::string& msg)
{
  showWarning(0, caption, msg);
}



//! Shows an error message.
/*!
  \param     caption Caption
  \param     msg Message.

  Use this function if the message is not related to a certain parent widget.
*/
void qt::AppWindow::showError(const std::string& caption, const std::string& msg)
{
  showError(0, caption, msg);
}



//! Shows an informative message.
/*!
  \param     p Parent widget.
  \param     caption Caption
  \param     msg Message.
*/
void qt::AppWindow::showInfo(QWidget* p, const std::string& caption,
                             const std::string& msg)
{
  QApplication::setOverrideCursor(Qt::ArrowCursor);
  QMessageBox::information(p, caption.c_str(), msg.c_str());
  QApplication::restoreOverrideCursor();
}



//! Shows a warning message.
/*!
  \param     p Parent widget.
  \param     caption Caption
  \param     msg Message.
*/
void qt::AppWindow::showWarning(QWidget* p, const std::string& caption,
                             const std::string& msg)
{
  QApplication::setOverrideCursor(Qt::ArrowCursor);
  QMessageBox::warning(p, caption.c_str(), msg.c_str());
  QApplication::restoreOverrideCursor();
}



//! Issue warning message and ask if Ok or to Cancel
/*!
  \param     p Parent widget.
  \param     caption Caption
  \param     explainConsequence should explain the consequences, see below.
  \return    true if Ok is pressed, falsed on Cancel.

  \a explainConsequence must explain what will happen when Ok or Cancel
     is pressed. Cancel is the default, and the Esc key will triggger Cancel.
     Make sure the operation is modelled as such, that Cancel is the least
     or non destructive option to choose from.

  Example:

  \code
  std::string m = "Closing the application now will terminate\n"
                  " the running scenario\n"
                  " Press Ok to terminate the scenario \n"
                  " Press Cancel to return to the application";
  if (confirmOkWarning(p, "app", m)) {
      // Terminate Scenario.
    }
  \endcode
*/
bool qt::AppWindow::confirmOkWarning(QWidget* p, const std::string& caption,
                             const std::string& explainConsequence)
{
  QApplication::setOverrideCursor(Qt::ArrowCursor);
  int b = QMessageBox::warning(p, caption.c_str(), explainConsequence.c_str(),
                           QMessageBox::Ok,
                           QMessageBox::Cancel | QMessageBox::Escape
                                               | QMessageBox::Default);
  QApplication::restoreOverrideCursor();
  return b == QMessageBox::Ok;
}



//! Shows an error message.
/*!
  \param     p Parent widget.
  \param     caption Caption
  \param     msg Message.
*/
void qt::AppWindow::showError(QWidget* p, const std::string& caption,
                             const std::string& msg)
{
  QApplication::setOverrideCursor(Qt::ArrowCursor);
  QMessageBox::critical(p, caption.c_str(), msg.c_str());
  QApplication::restoreOverrideCursor();
}



//------------------------------------------------------------------------------
// DEFINITION OF CLASS MEMBERS 
//------------------------------------------------------------------------------

//! Constructs an application window.
/*!
  \param   appName Name of the application window.
  \param   winName Name of the window.
  \param   version Version of the application window.
  \param   icon Icon of the window.
  \param   parent Parent widget.

  The difference between the name of the application window and the name of
  the window is subtle: More than one application window can have the same
  name (eg: 'Timeplot') but they should have different window names
  (eg: 'scen01', 'scen02', etc). The caption of the window will be based on
  the combination of the name of the application window and the name of the
  window (eg: 'Timeplot - scen01').
*/
qt::AppWindow::AppWindow(const std::string& appName,
                   const std::string& winName, const std::string& version,
                   const QPixmap& icon, QWidget* parent,
                   Qt::WindowFlags flags)

  : QMainWindow(parent, flags),
    d_winProps(appName, winName, version, icon)

{
  setAppName(appName);
  setWinName(winName);
  setVersion(version);
  // setWindowIcon(icon);
}



//! Constructs an application window.
/*!
  \param   properties Application window properties.
  \param   parent Parent widget.
*/
qt::AppWindow::AppWindow(const AppWindowProperties& properties,
                   QWidget* parent,
                   Qt::WindowFlags flags)

  : QMainWindow(parent, flags),
    d_winProps(properties)

{
  setAppName(properties.appName());
  setWinName(properties.winName());
  setVersion(properties.version());
  // setWindowIcon(properties.icon());
}



//! Constructs an application window.
/*!
  \param   properties Application window properties.
  \param   winName Name of the window.
  \param   parent Parent widget.

  \a winName overrules the window name in \a properties.
*/
qt::AppWindow::AppWindow(const AppWindowProperties& properties,
                   const std::string& winName,
                   QWidget* parent,
                   Qt::WindowFlags flags)

  : QMainWindow(parent, flags),
    d_winProps(properties)

{
  setAppName(properties.appName());
  setWinName(winName);
  setVersion(properties.version());
  // setWindowIcon(properties.icon());
}



//! Destructs an application window.
/*!
*/
qt::AppWindow::~AppWindow()
{
}



//! process closeEvent in a default way (???)
void qt::AppWindow::closeEvent(QCloseEvent* e)
{
  Q_EMIT closed(this);
  e->accept();
}



//! Quits the app. The default calls QApplication->quit().
/*!
*/
void qt::AppWindow::quit()
{
  qApp->quit();
}



//! Sets application window's name.
/*!
  \param   n Name of the application window.
*/
void qt::AppWindow::setAppName(const std::string& n)
{
  d_winProps.setAppName(n);

  std::string c;
  if(!appName().empty()) {
    c += appName();
  }
  if(!winName().empty()) {
    c += " - ";
    c += winName();
  }
  setWindowTitle(c.c_str());
  setWindowIconText(c.c_str());
}



//! Sets window's name.
/*!
  \param   n Name of the window.
*/
void qt::AppWindow::setWinName(const std::string& n)
{
  d_winProps.setWinName(n);

  std::string c;
  if(!appName().empty()) {
    c += appName();
  }
  if(!winName().empty()) {
    c += " - ";
    c += winName();
  }
  setWindowTitle(c.c_str());
  setWindowIconText(c.c_str());
}



//! Sets window's version.
/*!
  \param   version Version of application window.
*/
void qt::AppWindow::setVersion(const std::string& version)
{
  d_winProps.setVersion(version);
}



//! Shows an about box.
/*!
  \sa      setAbout()
  \bug     When the about box is open and the app window is closed, than the
           app dumps because the about box' parent is gone.

  The about box will show the name and version of the application window. If
  the optional about string is set (by calling setAbout()), than that will be
  added too.
*/
void qt::AppWindow::showAbout()
{
  std::ostringstream stream;
  stream << appName() /* << " for " << PLATFORM_TXT */ << '\n'
         << version() << '\n'
#ifdef DEBUG_DEVELOP
         << "PCRTEAM VERSION, INTERNAL USE ONLY!\n"
#endif
  ;

  if(!about().empty()) {
    stream << '\n' << about();
  }

  QMessageBox::about(this, QString(("About " + appName()).c_str()),
                   stream.str().c_str());
}



//! Shows a warning message.
/*!
  \param     m Warning message to show.
  \sa        showWarning(const std::string& , int&), showError(), showInfo()
*/
void qt::AppWindow::showWarning(const std::string& m) const
{
  showWarning(const_cast<AppWindow*>(this), appName().c_str(), m.c_str());
}



//! Issue warning message and ask if Ok or to Cancel
/*!
    shorthand for 
    qt::AppWindow::confirmOkWarning(QWidget* p, const std::string& app,
                  const std::string& msg), see there for further documentation.
 */
bool qt::AppWindow::confirmOkWarning(const std::string& m)
{
  return confirmOkWarning(this, appName().c_str(), m.c_str());
}



//! Shows an error message.
/*!
  \param     m Error message to show.
  \sa        showWarning(const std::string&),
             showWarning(const std::string&, int&), showInfo()
*/
void qt::AppWindow::showError(const std::string& m) const
{
  showError(const_cast<AppWindow*>(this), appName().c_str(), m.c_str());
}



//! Shows an information message.
/*!
  \param     m Information message to show.
  \sa        showWarning(const std::string&),
             showWarning(const std::string&, int&), showError()
*/
void qt::AppWindow::showInfo(const std::string& m) const
{
  showInfo(const_cast<AppWindow*>(this), appName().c_str(), m.c_str());
}



//! Returns the application window's name.
/*!
  \return    The name of the application window.
  \sa        winName(), version()
*/
const std::string& qt::AppWindow::appName() const
{
  return d_winProps.appName();
}



//! Returns the application window's version.
/*!
  \return    The version of the application window.
  \sa        appName(), winName()
*/
const std::string& qt::AppWindow::version() const
{
  return d_winProps.version();
}



//! Returns the application about text.
/*!
  \return    The about text of the application window.
  \sa        appName(), winName(), version()
*/
const std::string& qt::AppWindow::about() const
{
  return d_winProps.about();
}



//! Sets the about message to \a m.
/*!
  \param     m About message.
  \sa        showAbout()
*/
void qt::AppWindow::setAbout(const std::string& m)
{
  d_winProps.setAbout(m);
}



//! Returns the windows' name.
/*!
  \return    The name of the window.
  \sa        appName(), version()
*/
const std::string& qt::AppWindow::winName() const
{
  return d_winProps.winName();
}



const qt::AppWindowProperties& qt::AppWindow::windowProperties() const
{
  return d_winProps;
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

