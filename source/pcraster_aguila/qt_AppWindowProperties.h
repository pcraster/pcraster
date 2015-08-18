#ifndef INCLUDED_QT_APPWINDOWPROPERTIES
#define INCLUDED_QT_APPWINDOWPROPERTIES



// Library headers.
#include <string>
#include <QPixmap>

// PCRaster library headers.

// Module headers.



namespace qt {
  // AppWindowProperties declarations.
}



namespace qt {



//! The AppWindowProperties class contains information for application windows.
/*!
  Application windows have a name, window name and an icon.
*/
class AppWindowProperties
{

private:

  //! Name of the application window.
  std::string      d_appName;

  //! Name of the window.
  std::string      d_winName;

  //! Version of the application window.
  std::string      d_version;

  //! About text of the application window.
  std::string      d_about;

  //! Icon of the application window.
  QPixmap          d_icon;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   AppWindowProperties (const std::string& appName,
                                        const std::string& winName,
                                        const std::string& version,
                                        const std::string& about,
                                        const QPixmap& icon);

                   AppWindowProperties (const std::string& appName,
                                        const std::string& winName,
                                        const std::string& version,
                                        const QPixmap& icon);

                   AppWindowProperties (const std::string& appName,
                                        const std::string& version,
                                        const QPixmap& icon);

  /* virtual */    ~AppWindowProperties();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void             setAppName          (const std::string& appName);

  void             setWinName          (const std::string& winName);

  void             setVersion          (const std::string& version);

  void             setAbout            (const std::string& about);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  const std::string& appName           () const;

  const std::string& winName           () const;

  const std::string& version           () const;

  const std::string& about             () const;

  const QPixmap&   icon                () const;

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



} // namespace qt

#endif
