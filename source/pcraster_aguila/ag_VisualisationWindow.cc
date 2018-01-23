#include "ag_VisualisationWindow.h"

// Library headers.
#include <iomanip>
#include <set>
#include <QAction>
#include <QFileDialog>
#include <QMenu>
#include <QMenuBar>
#include <QProgressDialog>
#include <QToolBar>
#include <QToolButton>

// PCRaster library headers.
#include "dal_Exception.h"
#include "dal_FilesystemUtils.h"
#include "com_const.h"
#include "com_exception.h"
#include "qt_Accel.h"
#include "ag_Accel.h"
#include "ag_CursorWindow.h"
#include "ag_DataObject.h"
#include "ag_PreferencesDialog.h"
#include "ag_SaveViewAsDialog.h"
#include "ag_Viewer.h"
#include "ag_VisEngine.h"

// Module headers.
#include "icons/cursor.xpm"
// #include "icons/openlayer.xpm"
#include "icons/startanimation.xpm"



/*!
  \file
  This file contains the implementation of the VisualisationWindow class.
*/



namespace ag {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC VISUALISATIONWINDOW MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF VISUALISATIONWINDOW MEMBERS
//------------------------------------------------------------------------------

VisualisationWindow::VisualisationWindow(
         const qt::AppWindowProperties& props,
         const std::string& visualisationName,
         DataObject* object, Qt::WindowFlags flags)

  : qt::AppWindow(props, visualisationName, 0, flags),
    IVisualisation(object, visualisationName),
    d_fileMenu(0),
    d_editMenu(0),
    d_viewMenu(0),
    d_helpMenu(0),
    d_toolBar(0),
    d_animateAction(0),
    d_saveAsAction(0),
    d_preferencesAction(0)

{
}



VisualisationWindow::~VisualisationWindow()
{
}


void VisualisationWindow::insertAddAndNewVisualisationsMenu()
{
  QMenu* menu;

  menu = d_fileMenu->addMenu("&Add");
  menu->addAction("&Map View", this,
         SLOT(fileMenuAddMap2D()));
  menu->addAction("&Drape View", this,
         SLOT(fileMenuAddMap3D()));

  menu = d_fileMenu->addMenu("&New");
  menu->addAction("&Map View", this,
         SLOT(fileMenuNewMap2D()));
  menu->addAction("&Drape View", this,
         SLOT(fileMenuNewMap3D()));
}



// void VisualisationWindow::insertOpenAction()
// {
//   QPixmap openIcon = QPixmap((const char**)openlayer_xpm);
//   QAction* openAction = new QAction(
//          openIcon,
//          "&Open...",
//          this);
//   openAction->setShortcut(qt::Open);
//   connect(openAction,SIGNAL(triggered()),
//           this, SLOT(fileMenuOpen()));
//   d_fileMenu->addAction(openAction);
//   toolBar()->addAction(openAction);
// }


void VisualisationWindow::insertSaveAsMenuItem()
{
  d_saveAsAction = d_fileMenu->addAction("Save View As...", this,
         SLOT(fileMenuSaveAs()));
}



void VisualisationWindow::insertPreferencesMenuItem()
{
  d_preferencesAction = d_editMenu->addAction("Preferences...", this,
         SLOT(editMenuPreferences()));
}



void VisualisationWindow::insertAnimateAction()
{
  QPixmap animateIcon = QPixmap((const char**)startanimation_xpm);
  d_animateAction = new QAction(
         animateIcon,
         "&Animate...",
         this);
  d_animateAction->setShortcut(Animate);
  connect(d_animateAction, SIGNAL(triggered()), this,
         SLOT(fileMenuAnimationControl()));
  d_fileMenu->addAction(d_animateAction);
  toolBar()->addAction(d_animateAction);
}


void VisualisationWindow::insertCloseAndExitMenuItems()
{
  d_fileMenu->addAction("&Close", this, SLOT(fileMenuClose()), qt::Close);
  // d_fileMenu->insertItem("Close &group", this, SLOT(fileMenuCloseGroup()));

  if(qt::AppWindow::applicationRole() == qt::StandAlone) {
    d_fileMenu->addAction("E&xit", this, SLOT(quit()), qt::Exit);
  }
}



void VisualisationWindow::createFileMenu()
{
  d_fileMenu = menuBar()->addMenu("&File");
}



void VisualisationWindow::createEditMenu()
{
  assert(!d_editMenu);

  // PORT, menu must end up at position 1
  d_editMenu = menuBar()->addMenu("&Edit");
}



void VisualisationWindow::createViewMenu()
{
  assert(!d_viewMenu);

  // PORT, menu must end up at position 2
  d_viewMenu = menuBar()->addMenu("&View");
}



void VisualisationWindow::insertShowCursorAction()
{
  QPixmap icon = QPixmap((const char**)cursor_xpm);
  QAction* action = new QAction(
         icon,
         "Show &Cursor and Values...",
         this);
  connect(action, SIGNAL(triggered()), this,
         SLOT(viewMenuShowCursor()));
  d_viewMenu->addAction(action);
  toolBar()->addSeparator();
  toolBar()->addAction(action);
}



void VisualisationWindow::insertWhatsThisMenuItem()
{
  d_helpMenu->addAction(
         "What's &This",
         this,
         SLOT(whatsThis()),
         qt::WhatsThis);
}



void VisualisationWindow::insertAboutMenuItem()
{
  d_helpMenu->addAction(
         "About...",
         this,
         SLOT(showAbout()));
}



void VisualisationWindow::createHelpMenu()
{
  d_helpMenu = menuBar()->addMenu("&Help");
}



void VisualisationWindow::addToMenuAndToolBar(
         QMenu* menu,
         QAction* action,
         bool toggle)
{
  menu->addAction(action);
  toolBar()->addAction(action);
  menu->addAction(action);
  action->setCheckable(toggle);
}



/*!
 * \todo
 *   <ol>
 *    <li>A library such as pcrag may never call qApp->quit as it does 
 *        now in ag_visgroupmanager.cc: the quit application  logic must 
 *        be implemented such, that all software components agree on 
 *        quitting.
 *    <li>multiple About boxes? Other textual description of 
 *        Kor's achievements  within other COntexts?
 *   </ol>
 */
void VisualisationWindow::createInterface()
{
  d_toolBar = new QToolBar("Tool bar", this);
  addToolBar(Qt::TopToolBarArea, d_toolBar);

  createFileMenu();
  // insertAddAndNewVisualisationsMenu();
  // d_fileMenu->insertSeparator();
  // insertOpenAction();

  if(!fileFormats().empty()) {
    insertSaveAsMenuItem();
    d_fileMenu->addSeparator();
  }

  insertAnimateAction();

  d_fileMenu->addSeparator();
  insertCloseAndExitMenuItems();

  createEditMenu();
  insertPreferencesMenuItem();

  createViewMenu();
  insertShowCursorAction();

  createHelpMenu();
  // insertWhatsThisMenuItem();
  insertAboutMenuItem();

  rescan();
}



// void VisualisationWindow::fileMenuControlCenter()
// {
//   Q_EMIT showControlCenter();
// }



void VisualisationWindow::fileMenuAnimationControl()
{
  // Q_EMIT showAnimationControl();
  Viewer* viewer = Viewer::instance();
  VisGroup* group = viewer->group(this);
  viewer->displayAnimationDialog(group);
}



void VisualisationWindow::fileMenuNewMap2D()
{
  Q_EMIT newMap2DWindow(this);
}



void VisualisationWindow::fileMenuNewMap3D()
{
  Q_EMIT newMap3DWindow(this);
}



void VisualisationWindow::fileMenuNewTimePlot()
{
  Q_EMIT newTimePlotWindow(this);
}



void VisualisationWindow::fileMenuAddMap2D()
{
  Q_EMIT addMap2DWindow(this);
}



void VisualisationWindow::fileMenuAddMap3D()
{
  Q_EMIT addMap3DWindow(this);
}



void VisualisationWindow::fileMenuAddTimePlot()
{
  Q_EMIT addTimePlotWindow(this);
}



/*
void VisualisationWindow::fileMenuCopy()
{
  Q_EMIT copy(this);
}
*/



void VisualisationWindow::fileMenuOpen()
{
  QString filename = QFileDialog::getOpenFileName(this);

  try {
    if(!filename.isNull()) {
      VisGroup* group = Viewer::instance()->group(this);
      DataGuide guide = group->addData(
         std::string(filename.toUtf8().constData()), dal::DataSpace());
      addAttribute(guide);
      CursorWindow::instance(&dataObject())->addAttribute(guide);
      dataObject().notify();
    }
  }
  catch(dal::Exception const& exception) {
    qt::AppWindow::showError("Aguila", exception.message());
  }
  catch(const com::Exception &exception) {
    qt::AppWindow::showError("Aguila", exception.messages());
  }
}



void VisualisationWindow::fileMenuClose()
{
  // Since the Qt::WA_DeleteOnClose attribute is set in the window's
  // constructor, close will hide *and kill* the widget.
  QWidget::close();
}



void VisualisationWindow::fileMenuCloseGroup()
{
  Q_EMIT closeGroup();
}



void VisualisationWindow::fileMenuSaveAs()
{
  // Show save as dialog.
  // Query dialog for settings and perform actions.

  try {
    assert(!fileFormats().empty());

    dal::DataSpace space(dataSpace());
         // visualisationDataSpace().intersect(profileDataSpace(),
         // dal::DataSpace::DontIntersectCoordinates));
         // | dal::DataSpace::KeepNonSharedDimensions));
    dal::DataSpaceAddress address(space.trim(dataSpace(), dataSpaceAddress()));

    SaveViewAsDialog dialog(appName(), space, address, fileFormats(), this);

    if(dialog.exec() == QDialog::Accepted) {
      saveAs(dialog.selectedFormat(), dialog.name(), dialog.iterationSpace());
    }
  }
  catch(com::FileError const& exception) {
    showError(exception.messages());
  }
}



void VisualisationWindow::editMenuPreferences()
{
  PreferencesDialog(&dataObject(), this).exec();

  // if(dialog.exec() == QDialog::Accepted) {
  //   std::cout << "yahoo!" << std::endl;
  // }
}



void VisualisationWindow::viewMenuShowCursor()
{
  Viewer* viewer = Viewer::instance();
  VisGroup* group = viewer->group(this);
  viewer->displayCursor(group);
}



void VisualisationWindow::quit()
{
  qt_AppWindow::quit();

  // 20180123 This used to crash when Exit menu option is selected (
  //     Close menu option and Alt-Q shortcut worked fine...).
  //     Replaced with above line.
  // Q_EMIT closeAll();
}



//!
/*!
  \tparam    .
  \param     iterationSpace Space containing addresses for which the
             visualisation must be saved.
  \return    .
  \exception .
  \warning   .
  \sa        .
*/
void VisualisationWindow::saveAs(
         com::FileFormatInfo const& format,
         std::string name,
         dal::DataSpace iterationSpace)
{
  // dal::DataSpace space(dataObject().dataSpace());
  dal::DataSpaceAddress originalDataSpaceAddress(dataSpaceAddress());

  // Collection of (address, file name) tuples.
  typedef boost::tuple<dal::DataSpaceAddress, boost::filesystem::path>
         AddressPathTuple;
  std::vector<AddressPathTuple> addressPathTuples;

  assert(iterationSpace.nrWideDimensions() <= 1);
  assert(iterationSpace.nrWideDimensions() == 0 || iterationSpace.dimension(
         iterationSpace.indexOfWideDimension()).meaning() == dal::Time);

///   // If the iteration space contains scenarios, replace them with the name of
///   // the directory to write the output in. Otherwise add a scenario with this
///   // name.
  boost::filesystem::path path(dal::pathFor(name));
  std::string branchPath(path.parent_path().string());
  name = path.filename().string();
///   size_t index = iterationSpace.indexOf(dal::Scenarios);
/// 
/// std::cout << "has scenarios: " << (index < iterationSpace.rank()) << std::endl;
/// 
///   if(index < iterationSpace.rank()) {
///     assert(!branchPath.empty());
///     iterationSpace.dimension(index).setValue<std::string>(branchPath);
///   }
///   else if(!branchPath.empty()) {
///     std::set<std::string> names;
///     names.insert(branchPath);
///     iterationSpace.addDimension(dal::Dimension(dal::Scenarios, names));
///   }

  assert(!iterationSpace.isEmpty());

  // if(iterationSpace.isEmpty()) {
  //   addressPathTuples.push_back(AddressPathTuple(dataSpaceAddress(), name));
  // }
  for(dal::DataSpaceIterator it = iterationSpace.begin();
            it != iterationSpace.end(); ++it) {
    addressPathTuples.push_back(AddressPathTuple(
            *it, branchPath / dal::pathForDataSpaceAddress(name, iterationSpace, *it)));
  }

  assert(!addressPathTuples.empty());

  QProgressDialog progress("Saving", "Cancel",
         0, static_cast<int>(addressPathTuples.size()));
  progress.setModal(true);
  progress.show();
  progress.setValue(0);

  bool overwrite = false;

  for(size_t i = 0; i < addressPathTuples.size(); ++i) {

    if(progress.wasCanceled()) {
      break;
    }
    else {
      boost::filesystem::path path(boost::get<1>(addressPathTuples[i]));
      // dal::testPathIsWritable(path);

      // com::PathInfo pathInfo(boost::get<1>(addressPathTuples[i]));
      // pathInfo.testOpenForWriting();

      if(!overwrite && dal::exists(path)) {
        std::ostringstream stream;
        stream << "Overwrite existing file(s) '"
               << path.string() << "'?'";
                   // << pathInfo.pathName().baseName() << "'?";
        bool ok = confirmOkWarning(0, appName(), stream.str());

        if(!ok) {
          break;
        }
        else {
          overwrite = true;
        }
      }

      // Boom.
      dataObject().setDataSpaceAddress(boost::get<0>(addressPathTuples[i]));

      if(format == format.png()) {
        saveAsPNG(path);
      }
      else if(format == format.eps()) {
        saveAsEPS(path);
      }
      else {
        assert(false);
      }
    }

    progress.setValue(static_cast<int>(i + 1));
  }

  progress.setValue(static_cast<int>(addressPathTuples.size()));

  // Reset data space address.
  dataObject().setDataSpaceAddress(originalDataSpaceAddress);
}



// void VisualisationWindow::saveAs(
//          com::FileFormatInfo const& format,
//          com::PathName const& pathName,
//          bool allTimeSteps)
// {
//   assert(!pathName.isEmpty());
//   assert(pathName.hasExtension());
// 
//   dal::DataSpace space(dataObject().dataSpace());
//   dal::DataSpaceAddress const& address(dataObject().dataSpaceAddress());
//   size_t index;
// 
//   typedef boost::tuple<dal::DataSpaceAddress, std::string> AddressFilenameTuple;
//   std::vector<AddressFilenameTuple> addressFilenameTuples;
// 
//   if(!allTimeSteps || !space.hasTime()) {
//     dal::DataSpaceAddress tmpAddress(address);
// 
//     if(space.hasSpace()) {
//       index = space.indexOf(dal::Space);
//       tmpAddress.unsetCoordinate(index);
//       ++index;
//       assert(
//            space.dimension(index).meaning() == dal::Space);
//       tmpAddress.unsetCoordinate(index);
//     }
// 
//     addressFilenameTuples.push_back(
//          AddressFilenameTuple(tmpAddress, pathName.toString()));
//   }
//   else {
//     if(space.hasSpace()) {
//       index = space.indexOf(dal::Space);
//       space.dimension(index).clear();
//       ++index;
//       assert(
//            space.dimension(index).meaning() == dal::Space);
//       space.dimension(index).clear();
//     }
// 
//     std::string filename;
//     int width = com::intToStr(dataObject().lastTimeStep()).length();
//     index = space.indexOf(dal::Time);
// 
//     for(dal::DataSpaceIterator it(space.begin()); it != space.end(); ++it) {
//       std::ostringstream number;
//       number << std::setw(width) << std::setfill('0')
//          << (*it).coordinate<size_t>(index);
//       filename = pathName.toString();
//       filename.insert(filename.find_last_of('.'), number.str());
//       addressFilenameTuples.push_back(AddressFilenameTuple(*it, filename));
//     }
//   }
// 
//   QProgressDialog progress("Saving", "Cancel",
//          static_cast<int>(addressFilenameTuples.size()), 0,
//          "save as", true);
//   progress.show();
//   progress.setProgress(0);
// 
//   bool overwrite = false;
// 
//   for(size_t i = 0; i < addressFilenameTuples.size(); ++i) {
// 
//     if(progress.wasCancelled()) {
//       break;
//     }
//     else {
//       com::PathInfo pathInfo(boost::get<1>(addressFilenameTuples[i]));
//       pathInfo.testOpenForWriting();
// 
//       if(!overwrite && pathInfo.exists()) {
//         std::ostringstream stream;
//         stream << "Overwrite existing file(s) '"
//                    << pathInfo.pathName().baseName() << "'?";
//         bool ok = confirmOkWarning(0, appName(), stream.str());
// 
//         if(!ok) {
//           break;
//         }
//         else {
//           overwrite = true;
//         }
//       }
// 
//       dataObject().setDataSpaceAddress(boost::get<0>(addressFilenameTuples[i]));
// 
//       if(format == format.png()) {
//         saveAsPNG(pathInfo.pathName());
//       }
//       else if(format == format.eps()) {
//         saveAsEPS(pathInfo.pathName());
//       }
//       else {
//         assert(false);
//       }
//     }
// 
//     progress.setProgress(static_cast<int>(i + 1));
//   }
// 
//   progress.setProgress(static_cast<int>(addressFilenameTuples.size()));
// 
//   // Reset data space address.
//   dataObject().setDataSpaceAddress(address);
// }



bool VisualisationWindow::close()
{
  return qt::AppWindow::close();
}



/*
void VisualisationWindow::toggleFullScreen()
{
  if(d_data->d_isFullScreen) {
    if(d_data->d_fullScreenTB) {
      d_data->d_fullScreenTB->setOn(false);
    }

    d_data->d_isFullScreen = false;
    showNormal();
  }
  else {
    if(d_data->d_fullScreenTB) {
      d_data->d_fullScreenTB->setOn(true);
    }

    d_data->d_isFullScreen = true;
    showFullScreen();
  }
}
*/



/*!
 * \todo
 *   full screen is gewenst door gebruikers, waarbij evenredig uitvergroot
 *   wordt; m.a.w. hetzelfde deel van de kaar blijft zichtbaar.
 */
 /*
void Visualisation::createFullScreenToolButton(QToolBar* toolBar)
{
  QPixmap fullScreenIcon = QPixmap((const char**)fullscreen_xpm);
  d_data->d_fullScreenTB = new QToolButton(fullScreenIcon,
         "Full screen", "Full screen mode", this, SLOT(toggleFullScreen()),
         toolBar, "full screen");
  d_data->d_fullScreenTB->setToggleButton(false);
}
*/


void VisualisationWindow::setEnableAnimation(bool enable)
{
  if(d_animateAction) {
    d_animateAction->setEnabled(enable);
  }
}



void VisualisationWindow::setEnableSaveAs(bool enable)
{
  if(d_saveAsAction) {
    d_saveAsAction->setEnabled(enable);
  }
}



std::string VisualisationWindow::windowName() const
{
  return visualisationName();
}



bool VisualisationWindow::dataVisualised() const
{
  return false;
}



void VisualisationWindow::rescan()
{
  setWinName(windowName());
  setEnableAnimation(dataObject().timeSpan() > 0);
  setEnableSaveAs(dataVisualised());
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace ag



