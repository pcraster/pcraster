#include "ag_Aguila.h"
#include <fstream>
#include <boost/filesystem.hpp>
#include "pcrxsd_library.h"
#include "dev_FilesystemUtils.h"

#ifdef DEBUG_DEVELOP
#include "AguilaXSD.h"  //  only for test.xml
#endif
#include "dal_Dal.h"
#include "qt_AppWindow.h"
#include "ag_Viewer.h"
#include "ag_AguilaProgramOptions.h"
#include "icons/pcr_16x16.xpm"



namespace ag {

namespace detail {

std::string const DEVELOPERMESSAGE(
         "Copyright (C) PCRaster R&D team\n"
         "Department of Physical Geography\n"
         "Faculty of Geosciences\n"
         "Utrecht University\n"
         "and PCRaster Environmental Software\n"
         "The Netherlands");
}

//------------------------------------------------------------------------------
// DEFINITION OF STATIC CLASS MEMBERS
//------------------------------------------------------------------------------

// static void showError(char const* msg)
// {
//   qt::AppWindow::showError("Aguila", msg);
// }



//------------------------------------------------------------------------------
// DEFINITION OF CLASS MEMBERS 
//------------------------------------------------------------------------------


//! Constructs an Aguila application object.
/*!
  \param     argc Argument count.
  \param     argv Argument vector.
  \param     role Role the application plays in the process.

*/
Aguila::Aguila(
         int& argc,
         char** argv,
         qt::ApplicationRole role)

  : qt::GuiApp(argc, argv,
         /* com::CommandLine("Aguila", __DATE__, argv[0]), com::GNU, */ role),
    dev::GDalClient(),
    dal::Client(dev::prefix(argv[0]), true),
    d_xsdLib(new pcrxsd::Library()),
    d_argc(argc),
    d_argv(argv),
    d_viewer(0)

{
  assert(dev::GDalClient::isInitialized());
  assert(dal::Client::isInitialized());

  // List drivers our users most likely need.
  std::vector<std::string> drivers;
  drivers.push_back("CSF");
  drivers.push_back("ESRI Shapefile");
  drivers.push_back("HDF4Image");
  drivers.push_back("HDF4");
  drivers.push_back("GML");
  drivers.push_back("KML");
  dal::Client::dal().favourDrivers(drivers);

  applyGplLicense("PCRaster R&D team");
  setVersion(AGUILA_MAJOR_VERSION, AGUILA_MINOR_VERSION, AGUILA_PATCH_VERSION);
  // setBuildStage(AG_BUILD_STAGE);

  qt::AppWindow::setApplicationRole(role);
  // QApplication::setWindowIcon(QIcon(QPixmap((char const **)pcr_16x16_xpm)));

  init(qt::AppWindowProperties("Aguila", "", version(),
         detail::DEVELOPERMESSAGE + "\n\n" + license(),
         QPixmap(pcr_16x16_xpm)));
}



//! Destructs an Aguila application object.
/*!
*/
Aguila::~Aguila()
{
  Viewer::resetInstance();
  delete d_xsdLib;
}



void Aguila::init(
         qt::AppWindowProperties const& awp)
{
  // setParseCommandLine(false);

  // old PCRaster C lib error handler (lib misc)
  // errorHandler = ag::showError;

  // // Don't create the viewer before calling dal::Library::initialise().
  // dal::Library::initialise();
  d_viewer = Viewer::instance(awp);
  assert(d_viewer);

  // QWidget* dummy = new QWidget();
  // SoQt::init(dummy);
}



void Aguila::setup()
{
  AguilaProgramOptions apo(d_argc, d_argv);

  if(!apo.help().empty())
      showInfo(apo.help());
  if(apo.license())
      showInfo(license());
  if(apo.version())
      showInfo(version());

  // createLockFile();
  // parseConfigurationFile();

/// #ifdef DEBUG_DEVELOP
///   // TEST
/// 
///   char arg1[5] = "test";
///   char arg2[3] = "-x";
/// #ifdef __linux__
///   char arg3[14] = "/tmp/test.xml";
/// #else
///   char arg3[9] = "test.xml";
/// #endif
///   char *argv[3] = { arg1, arg2, arg3 };
/// 
///   std::ofstream f(arg3);
///   pcrxml::aguila(f, apo.configuration(),
///                  pcrxsd::namespaceInfoMap("Aguila.xsd"));
///   f.close();
/// 
///   AguilaProgramOptions p(3, argv);
///   viewer().createViews(p.configuration());
/// #else
  viewer().createViews(apo.configuration());
/// #endif
}



Viewer& Aguila::viewer()
{
  assert(d_viewer);
  return *d_viewer;
}



// Aguila::Aguila()
//   : qt::GuiApp(com::CommandLine("Aguila", __DATE__, "aguila"), com::GNU),
//     d_data(new AguilaPrivate(qt::AppWindowProperties("Aguila", "", __DATE__,
//          com::DEVELOPERMESSAGE_KOR, (char const **)pcr_180x39_xpm)))
// {
//   init();
//   assert(d_data);
//   assert(d_viewer);
// }

// void Aguila::createLockFile()
// {
//   // if(d_data->d_lockFileArg.isParsed()) {
//   //   // Will be deleted in qt::GuiApp's destructor.
//   //   qt::GuiApp::createLockFile(d_data->d_lockFileArg.value());
//   // }
// }
// 
// void Aguila::parseConfigurationFile()
// {
//   // if(d_data->d_configFileArg.isParsed()) {
//   //   /*
//   //   d_data->d_manager->loadSettings(com::PathName(
//   //                  d_data->d_configFileArg.value()));
//   //   */
//   // }
// }



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS 
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS 
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DOCUMENTATION OF ENUMERATIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DOCUMENTATION OF INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DOCUMENTATION OF PURE VIRTUAL FUNCTIONS
//------------------------------------------------------------------------------

} // namespace ag

