#include "qt_AppWindowProperties.h"

// Library headers.

// PCRaster library headers.

// Module headers.



/*!
  \file
  This file contains the implementation of the AppWindowProperties class.
*/



//------------------------------------------------------------------------------

/*
namespace qt {

class AppWindowPropertiesPrivate
{
public:

  AppWindowPropertiesPrivate()
  {
  }

  ~AppWindowPropertiesPrivate()
  {
  }

};

} // namespace qt
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC APPWINDOWPROPERTIES MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF APPWINDOWPROPERTIES MEMBERS
//------------------------------------------------------------------------------

qt::AppWindowProperties::AppWindowProperties(
                   const std::string& appName, const std::string& winName,
                   const std::string& version, const std::string& about,
                   const QPixmap& icon)

  : d_appName(appName), d_winName(winName), d_version(version), d_about(about),
    d_icon(icon)

{
}



qt::AppWindowProperties::AppWindowProperties(
                   const std::string& appName, const std::string& winName,
                   const std::string& version, const QPixmap& icon)

  : d_appName(appName), d_winName(winName), d_version(version), d_icon(icon)

{
}



qt::AppWindowProperties::AppWindowProperties(const std::string& appName,
                   const std::string& version, const QPixmap& icon)

  : d_appName(appName), d_version(version), d_icon(icon)

{
}



qt::AppWindowProperties::~AppWindowProperties()
{
}



void qt::AppWindowProperties::setAppName(const std::string& appName)
{
  d_appName = appName;
}



void qt::AppWindowProperties::setWinName(const std::string& winName)
{
  d_winName = winName;
}



void qt::AppWindowProperties::setVersion(const std::string& version)
{
  d_version = version;
}



void qt::AppWindowProperties::setAbout(const std::string& about)
{
  d_about = about;
}



const std::string& qt::AppWindowProperties::appName() const
{
  return d_appName;
}



const std::string& qt::AppWindowProperties::winName() const
{
  return d_winName;
}



const std::string& qt::AppWindowProperties::version() const
{
  return d_version;
}



const std::string& qt::AppWindowProperties::about() const
{
  return d_about;
}



const QPixmap& qt::AppWindowProperties::icon() const
{
  return d_icon;
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



