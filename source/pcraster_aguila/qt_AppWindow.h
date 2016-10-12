#ifndef INCLUDED_QT_APPWINDOW
#define INCLUDED_QT_APPWINDOW



#include <string>
#include <QMainWindow>
#include "qt_AppWindowProperties.h"
#include "qt_Def.h"



class QCloseEvent;
class QPixmap;
namespace qt {
}



namespace qt {

/*!
  \class AppWindow
  \brief The AppWindow class is for (toplevel) application window objects.

  AppWindow objects are QMainWindow objects with some added functionality.

  An application window is one of possibly more toplevel windows belonging to
  an application. An application can have different application windows with
  different names and different versions.

  \sa GuiApp
*/
class AppWindow: public QMainWindow
{

private:

  static ApplicationRole d_role;

public:

  static void      setApplicationRole  (ApplicationRole role);

  static ApplicationRole applicationRole();

  static void      showInfo            (const std::string& app,
                                        const std::string& msg);

  static void      showInfo            (QWidget* p,
                                        const std::string& app,
                                        const std::string& msg);

  static void      showWarning         (const std::string& caption,
                                        const std::string& msg);

  static void      showWarning         (QWidget* p,
                                        const std::string& caption,
                                        const std::string& msg);

  static bool      confirmOkWarning    (QWidget* p,
                                        const std::string& caption,
                                        const std::string& explainConsequence);

  static void      showError           (const std::string& caption,
                                        const std::string& msg);

  static void      showError           (QWidget*           p,
                                        const std::string& caption,
                                        const std::string& msg);

private:

  Q_OBJECT

  //! Name of the application window.
  std::string      d_appName;

  //! Name of the window.
  std::string      d_winName;

  //! Version of the application window.
  std::string      d_version;

  //! Application window properties.
  AppWindowProperties d_winProps;

  //! Assignment operator. NOT IMPLEMENTED.
  AppWindow&       operator=           (const AppWindow&);

  //! Copy constructor. NOT IMPLEMENTED.
                   AppWindow           (const AppWindow&);

  void             setAppName          (const std::string& n);

  void             setVersion          (const std::string& version);

protected Q_SLOTS:

  void             showAbout           ();

  virtual void     quit                ();

protected:

  void             closeEvent          (QCloseEvent* e);

  void             setAbout            (const std::string& about);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   AppWindow           (const std::string& appName,
                                        const std::string& windowName,
                                        const std::string& version,
                                        const QPixmap& icon,
                                        QWidget* parent = 0,
                                        Qt::WindowFlags flags = Qt::Window);

                   AppWindow           (const AppWindowProperties& properties,
                                        QWidget* parent = 0,
                                        Qt::WindowFlags flags = Qt::Window);

                   AppWindow           (const AppWindowProperties& properties,
                                        const std::string& windowName,
                                        QWidget* parent = 0,
                                        Qt::WindowFlags flags = Qt::Window);

  virtual          ~AppWindow          ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void             setWinName          (const std::string& n);

  void             showWarning         (const std::string& m) const;

  bool             confirmOkWarning    (const std::string& m);

  void             showError           (const std::string& m) const;

  void             showInfo            (const std::string& m) const;

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  const std::string& appName           () const;

  const std::string& winName           () const;

  const std::string& version           () const;

  const std::string& about             () const;

  const AppWindowProperties& windowProperties() const;

Q_SIGNALS:

  //! Gets emitted when the application is closed.
  void             closed              (qt::AppWindow* w);

};



//------------------------------------------------------------------------------
// INLINE FUNCIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



} // namespace

typedef qt::AppWindow qt_AppWindow;

#endif
