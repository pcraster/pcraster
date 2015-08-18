#include <QToolBar>
#include "ag_Map2DWindow.h"
#include "ag_DataObject.h"
#include "ag_Map2D.h"
#include "ag_VisEngine.h"



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

ag::Map2DWindow::Map2DWindow(const qt::AppWindowProperties& props,
         ag::DataObject* dataObject)

  : MapWindow(props, "Map Window", dataObject),
    d_map(0)

{
  std::vector<com::FileFormatInfo> fileFormats;
  fileFormats.push_back(com::FileFormatInfo::png());
  setSaveAsFileFormats(fileFormats);

  // dal::DataSpace space;
  // space.addDimension(dal::Dimension(dal::Time));
  // setProfileDataSpace(space);

  createInterface();
}



ag::Map2DWindow::~Map2DWindow()
{
}



/*!
 * \todo
 *   Inspect is a better term then the (techical) query
 */
void ag::Map2DWindow::createInterface()
{
  MapWindow::createInterface();

  // viewMenu()->insertSeparator();
  /// toolBar()->addSeparator();

  d_map = new Map2D(&dataObject(), this);

  /// addMapActionGroup();

/*
  QAction* selectAction = new QAction("Start Select Mode", selectIcon,
         "&Select", QKeySequence(), ag, "start select mode");
  connect(selectAction,SIGNAL(triggered()), d_map,
         SLOT(startSelectMode()));
  addToInterface(selectAction);
*/

  addZoomAllAction(d_map);

  setCentralWidget(d_map);

  rescan();
}



void ag::Map2DWindow::rescan()
{
  visualisationEngine().rescan(dataObject());
  MapWindow::rescan();
}



void ag::Map2DWindow::addAttribute(
         ag::DataGuide const& guide)
{
  visualisationEngine().addAttribute(dataObject(), guide);
  d_map->addAttribute(guide);
}



std::string ag::Map2DWindow::windowName() const
{
  std::string name = "No data loaded";
  std::vector<DataGuide> dataGuides = visualisationEngine().dataGuides();

  if(!dataGuides.empty()) {
    name = dataObject().description(dataGuides[0]);
    for(size_t i = 1; i < dataGuides.size(); ++i) {
      name += " + " + dataObject().description(dataGuides[i]);
    }
  }

  return name;
}



bool ag::Map2DWindow::dataVisualised() const
{
  return visualisationEngine().dataGuides().size() > 0;
}



void ag::Map2DWindow::saveAsPNG(
         boost::filesystem::path const& path)
{
  d_map->saveAsPNG(path);
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


