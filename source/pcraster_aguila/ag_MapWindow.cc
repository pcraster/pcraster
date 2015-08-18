#include "ag_MapWindow.h"
#include <QAction>
#include <QMenu>
#include <QMenuBar>
#include <QToolBar>
#include "ag_DataObject.h"
#include "icons/zoomall.xpm"



/*!
  \file
  brief

  more elaborated
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC CLASS MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF CLASS MEMBERS 
//------------------------------------------------------------------------------

ag::MapWindow::MapWindow(const qt::AppWindowProperties& props,
         const std::string& windowName, ag::DataObject* dataObject)

  : VisualisationWindow(props, windowName, dataObject, Qt::Window),
    /* d_queryAction(0), d_panAction(0), d_zoomAction(0), */
    d_zoomAllAction(0)

{
}



ag::MapWindow::~MapWindow()
{
}



/// void ag::MapWindow::addMapActionGroup()
/// {
///   QPixmap queryIcon = QPixmap((const char**)point_xpm);
///   QPixmap selectIcon = QPixmap((const char**)select_xpm);
///   QPixmap panIcon = QPixmap((const char**)pan_xpm);
///   QPixmap zoomAreaIcon = QPixmap((const char**)zoomarea_xpm);
/// 
///   QActionGroup* actionGroup = new QActionGroup(this);
///   actionGroup->setExclusive(true);
/// 
///   d_queryAction = new QAction(queryIcon, "&Query", this);
///   d_queryAction->setText("Start Query Mode");
///   d_queryAction->setActionGroup(actionGroup);
///   connect(d_queryAction, SIGNAL(triggered()),
///           &dataObject(), SLOT(startQueryMode()));
///   addToMenuAndToolBar(viewMenu(), d_queryAction, true);
/// 
///   d_panAction = new QAction(panIcon, "&Pan", this);
///   d_panAction->setText("Start Pan Mode");
///   d_panAction->setActionGroup(actionGroup);
///   connect(d_panAction, SIGNAL(triggered()),
///           &dataObject(), SLOT(startPanMode()));
///   addToMenuAndToolBar(viewMenu(), d_panAction, true);
/// 
///   d_zoomAction = new QAction(zoomAreaIcon, "&Zoom Area", this);
///   d_zoomAction->setText("Start Zoom Area Mode");
///   d_zoomAction->setActionGroup(actionGroup);
///   connect(d_zoomAction, SIGNAL(triggered()),
///           &dataObject(), SLOT(startZoomAreaMode()));
///   addToMenuAndToolBar(viewMenu(), d_zoomAction, true);
/// }



void ag::MapWindow::addZoomAllAction(QObject* receiver)
{
  QPixmap zoomAllIcon =  QPixmap((const char**)zoomall_xpm);

  viewMenu()->addSeparator();
  toolBar()->addSeparator();

  // STRANGE, make viewMenu the parent, as it should be
  // will yield an access violation when the application is exited
  d_zoomAllAction = new QAction(zoomAllIcon, "&Reset Map View", this);
  d_zoomAllAction->setText("Reset Map View");
  connect(d_zoomAllAction, SIGNAL(triggered()), receiver, SLOT(resetMapView()));
  addToMenuAndToolBar(viewMenu(), d_zoomAllAction, false);
}



void ag::MapWindow::createInterface()
{
  VisualisationWindow::createInterface();
  viewMenu()->addSeparator();
}



/// void ag::MapWindow::rescanMapAction()
/// {
///   if(d_queryAction && d_panAction && d_zoomAction) {
///     switch(dataObject().mapAction()) {
///       case QUERY: {
///         d_queryAction->setChecked(true);
///         break;
///       }
///       case PAN: {
///         d_panAction->setChecked(true);
///         break;
///       }
///       case ZOOM_AREA: {
///         d_zoomAction->setChecked(true);
///         break;
///       }
///       default: {
///         assert(false);
///         break;
///       }
///     }
///   }
/// }



void ag::MapWindow::setEnableDataInterfaceElements(bool enable)
{
  if(/* d_queryAction && d_panAction && d_zoomAction && */ d_zoomAllAction) {
    /// d_queryAction->setEnabled(enable);
    /// d_panAction->setEnabled(enable);
    /// d_zoomAction->setEnabled(enable);
    d_zoomAllAction->setEnabled(enable);
  }
}



void ag::MapWindow::rescan()
{
  VisualisationWindow::rescan();
  /// rescanMapAction();
  setEnableDataInterfaceElements(dataVisualised());
}



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


