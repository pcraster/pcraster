#include "ag_Map2D.h"
#include <QLayout>
#include <QSplitter>
#include "qt_Const.h"
#include "com_exception.h"
#include "ag_DataObject.h"
#include "ag_LegendView.h"
#include "ag_Map2DView.h"
#include "ag_VisEngine.h"



/*!
  \file
  brief

  more elaborated
*/


namespace ag {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC CLASS MEMBERS
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// DEFINITION OF CLASS MEMBERS 
//------------------------------------------------------------------------------

//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .

  Default mode is query mode.
*/
Map2D::Map2D(DataObject* object, QWidget* parent)

  : Map(object, "2D Map", parent),
    d_mapView(0), d_legendView(0)

{
  createInterface();
}



Map2D::~Map2D()
{
}



void Map2D::createInterface()
{
  d_splitter = new QSplitter(Qt::Horizontal, this);
  QVBoxLayout* layout = new QVBoxLayout(this);
  layout->addWidget(d_splitter);

  d_legendView = new LegendView(&dataObject(), VT_Map, d_splitter);
  d_mapView = new Map2DView(&dataObject(), d_splitter);

  // Set resize modes and initial sizes of the views.
  d_splitter->setStretchFactor(d_splitter->indexOf(d_legendView), 0);
  d_splitter->setStretchFactor(d_splitter->indexOf(d_mapView), 1);
  d_splitter->setHandleWidth(5);

  QList<int> sizes;
  sizes.append(100);
  sizes.append(500);
  d_splitter->setSizes(sizes);
}



/*
void Map2D::showEvent(QShowEvent* event)
{
  if(!event->spontaneous()) {
    // We are shown on purpose. Since we have been sleeping untill now we must
    // rescan to find out what we should do.

    // If this is the first time the map view is configured with data, than
    // we have to set the scale of the view, otherwise nothing will be shown.
    if(d_mapView->scale() == 0.0) {
      d_mapView->zoomAll();
    }

    rescan();
  }
}
*/



void Map2D::addAttribute(const DataGuide& dataGuide)
{
  d_mapView->addAttribute(dataGuide);
  d_legendView->addAttribute(dataGuide);
}



/*
void Map2D::clear()
{
  d_mapView->clear();
  d_legendView->clear();

  d_legendView->rescan();
  d_mapView->rescan();
}
*/



const LegendView* Map2D::legendView() const
{
  return d_legendView;
}



void Map2D::saveAsPNG(
         boost::filesystem::path const& path) const
{
  QPixmap const& buffer(static_cast<Map2DView const*>(d_mapView)->buffer());

  if(buffer.isNull()) {
    throw com::FileError(path.string(), "Error while saving");
  }

  if(!buffer.save(QString(path.string().c_str()), "PNG")) {
    throw com::FileError(path.string(), "Error while saving");
  }
}



void Map2D::startQueryMode()
{
  d_mapView->startQueryMode();
}



void Map2D::startPanMode()
{
  d_mapView->startPanMode();
}



void Map2D::startZoomAreaMode()
{
  d_mapView->startZoomAreaMode();
}



void Map2D::startSelectMode()
{
  d_mapView->startSelectMode();
}



void Map2D::resetMapView()
{
  d_mapView->resetMapView();
}



void Map2D::rescan()
{
  visualisationEngine().rescan(dataObject());
}



void Map2D::process()
{
  if(visualisationEngine().change() & VisEngine::BACKGROUND_COLOUR) {
    if(!dataObject().backgroundColour().isValid()) {
      setPalette(QPalette());
    }
    else {
      QPalette palette;
      palette.setColor(backgroundRole(), dataObject().backgroundColour());
      setPalette(palette);
    }
  }
}



void Map2D::visualise()
{
  // Done scanning, update stuff if needed.
  if(visualisationEngine().change() & VisEngine::BACKGROUND_COLOUR) {
    d_splitter->update();
  }

  visualisationEngine().finishedScanning(dataObject());
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

} // namespace ag
