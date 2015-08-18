#include "ag_Map3D.h"
#include <QLayout>
#include <QPixmap>

// Library headers.
#include <boost/format.hpp>
#include <QSplitter>

// PCRaster library headers.
#include "com_exception.h"
#include "qt_Const.h"

// Module headers.
#include "ag_DataObject.h"
#include "ag_LegendView.h"
#include "ag_Map3DView.h"
#include "ag_VisEngine.h"



/*!
  \file
  This file contains the implementation of the Map3D class.
*/



namespace ag {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC MAP3D MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF MAP3D MEMBERS
//------------------------------------------------------------------------------

Map3D::Map3D(DataObject* object, QWidget* parent)

  : Map(object, "3D Map", parent),
    d_mapView(0), d_legendView(0)

{
  createInterface(object);
}



Map3D::~Map3D()
{
}



void Map3D::createInterface(DataObject* object)
{
  d_splitter = new QSplitter(Qt::Horizontal, this);
  QVBoxLayout* layout = new QVBoxLayout(this);
  layout->addWidget(d_splitter);

  d_legendView = new LegendView(object, VT_Map, d_splitter);
  d_mapView = new Map3DView(object, d_splitter);

  // Set resize modes and initial sizes of the views.
  d_splitter->setStretchFactor(d_splitter->indexOf(d_legendView), 0);
  d_splitter->setStretchFactor(d_splitter->indexOf(d_mapView), 1);
  d_splitter->setHandleWidth(5);

  QList<int> sizes;
  sizes.append(100);
  sizes.append(500);
  d_splitter->setSizes(sizes);
}



void Map3D::addAttribute(const DataGuide& dataGuide)
{
  d_mapView->addAttribute(dataGuide);
  d_legendView->addAttribute(dataGuide);
}



void Map3D::setHeight(const DataGuide& dataGuide)
{
  d_mapView->setHeight(dataGuide);
}



int Map3D::depthOfRenderingContext() const
{
  return d_mapView->depthOfRenderingContext();
}



bool Map3D::doubleBuffer() const
{
  return d_mapView->doubleBuffer();
}



void Map3D::saveAsPNG(
         boost::filesystem::path const& path) const
{
  if(QPixmap::defaultDepth() != d_mapView->depthOfRenderingContext()) {
    std::string msg = (boost::format(
         "Error while saving\n"
         "Please make sure that the depth of the desktop equals the depth\n"
         "of the OpenGL rendering context. Currently the depth of the\n"
         "desktop is %1% bits while the depth of the OpenGL rendering context is %2% bits.")
              % QPixmap::defaultDepth()
              % d_mapView->depthOfRenderingContext()).str();
    throw com::FileError(path.string().c_str(), msg);
  }

  // Retrieve drawing.
  QPixmap map = d_mapView->renderPixmap();
  if(map.isNull() || (!map.save(QString(path.string().c_str()), "PNG"))) {
    throw com::FileError(path.string(), "Error while saving");
  }
}



void Map3D::rescan()
{
  visualisationEngine().rescan(dataObject());
}



void Map3D::process()
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



void Map3D::visualise()
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

} // namespace ag

