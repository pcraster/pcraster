#include "ag_Map3DWindow.h"
#include <sstream>
#include <boost/format.hpp>
#include <QMenu>
#include "com_exception.h"
#include "ag_DataObject.h"
#include "ag_Map3D.h"
#include "ag_VisEngine.h"



//------------------------------------------------------------------------------

namespace ag {

/*
class ViewData
{
public:
  QPoint           d_press;            // Mouse position after mouse press.
  QPoint           d_move;             // Mouse position after mouse move.
};

class Map3DWindowPrivate
{
public:
  Map3DView*       d_mapView;          // Map view with drape view.
  ViewData         d_mapViewData;      // View related data.
  QPopupMenu *     d_mapViewPopup;     // Map view popup.
  Map3DViewOptionsDlg *d_mapViewDlg;   // Dialog for editing scene options.

  LegendView *     d_legendView;       // Legend view for drape view.

  CursorView *     d_cursorView;       // Cursor view for drape view 

  // Map3DEngine      d_engine;           // Engine for manipulating the views.

  Map3DWindowPrivate()
    : d_mapView(0), d_mapViewPopup(0), d_mapViewDlg(0),
      d_legendView(0), d_cursorView(0)
  {
  }

  ~Map3DWindowPrivate()
  {
  }

};
*/

}

//------------------------------------------------------------------------------
// DEFINITION OF STATIC CLASS MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF CLASS MEMBERS 
//------------------------------------------------------------------------------

ag::Map3DWindow::Map3DWindow(const qt::AppWindowProperties& props,
         DataObject* object)

  : MapWindow(props, "Drape View", object)

{
  std::vector<com::FileFormatInfo> fileFormats;
  fileFormats.push_back(com::FileFormatInfo::png());
  setSaveAsFileFormats(fileFormats);

  createInterface();
}



ag::Map3DWindow::~Map3DWindow()
{
}



void ag::Map3DWindow::createInterface()
{
  MapWindow::createInterface();

  helpMenu()->addAction("OpenGL info...", this, SLOT(showOpenGLInfo()));

  d_map = new Map3D(&dataObject(), this);

  setCentralWidget(d_map);
}



void ag::Map3DWindow::rescan()
{
  visualisationEngine().rescan(dataObject());
  MapWindow::rescan();
}



void ag::Map3DWindow::addAttribute(const ag::DataGuide& dataGuide)
{
  visualisationEngine().addAttribute(dataObject(), dataGuide);
  d_map->addAttribute(dataGuide);
}



//!
/*!
  \param     .
  \return    .
  \exception com::Exception When VisEngine::setHeight(DataGuide const&) throws.
  \warning   .
  \sa        .
*/
void ag::Map3DWindow::setHeight(
         ag::DataGuide const& dataGuide)
{
  try {
    visualisationEngine().setHeight(dataGuide);
    d_map->setHeight(dataGuide);
  }
  catch(com::Exception& exception) {
    std::string message = (boost::format(
         "Unable to set %1% as height data for %2% visualisation:")
         % dataObject().description(dataGuide)
         % visualisationName()).str();
    exception.prepend(message);
    throw;
  }
}



void ag::Map3DWindow::saveAsPNG(
         boost::filesystem::path const& path)
{
  d_map->saveAsPNG(path);
}



/*
void ag::Map3DWindow::saveAsEPS(const com::PathName& pathName)
{
  // size = tokens + nrvertices + nrvertices * info per vertice
  //      = nrquads * 2 triangles +
  //        nrquads * 2 triangles +
  //        nrquads * 2 triangles * 3 vertices per triangle *
  //               (3 coordinates per vertice + 4 color values per vertice)
  //      = nrquads * 2 triangles * 2 +
  //               nrquads * 2 triangles * 3 vertices * 7 elements per vertex
  //      = nrquads * 46

  size_t size = 0;
  if(dataObject().clone()) {
    // Here we assume that if a clone is present, that we are drawing stacks.
    // This needn't be the case: stack added -> clone set -> all stacks removed
    // clone still set. We keep on the save side but it will cost some memory.
    size += (dataObject().clone()->nrCells() * 46);
  }

  Feedback feedback(size);
  GLfloat viewport[4];
  glGetFloatv(GL_VIEWPORT, viewport);

  d_data->d_mapView->retrieveFeedback(&feedback);
  PostScript postscript(feedback, viewport[0], viewport[1], viewport[2],
                   viewport[3]);
  postscript.save(pathName);
}
*/



void ag::Map3DWindow::showOpenGLInfo()
{
  std::ostringstream stream;
  stream
    << "<table>"
    << "<tr><th>Property</th><th>Value</th></tr>"
    << "<tr><td>Depth of rendering context</td><td>"
         << d_map->depthOfRenderingContext() << "</td>"
    << "<tr><td>Double buffering</td><td>"
         << (d_map->doubleBuffer() ? "yes" : "no") << "</td></tr>"
    << "</table>";

  showInfo(stream.str());
}



std::string ag::Map3DWindow::windowName() const
{
  std::string name = "No data loaded";

  if(visualisationEngine().heightDataGuide()) {
    name = dataObject().name(*visualisationEngine().heightDataGuide());
    std::vector<DataGuide> dataGuides = visualisationEngine().dataGuides();

    for(size_t i = 0; i < dataGuides.size(); ++i) {
      name += " + " + dataObject().name(dataGuides[i]);
    }
  }

  return name;
}



bool ag::Map3DWindow::dataVisualised() const
{
  return visualisationEngine().heightDataGuide() != 0;
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


