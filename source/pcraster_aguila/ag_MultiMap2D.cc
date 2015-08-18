#include "ag_MultiMap2D.h"

// Library headers.
#include <QLayout>
#include <QSplitter>

// PCRaster library headers.
#include "qt_Const.h"

// Module headers.
#include "ag_DataObject.h"
#include "ag_LegendView.h"
#include "ag_MultiMap2DView.h"
#include "ag_VisEngine.h"



/*!
  \file
  This file contains the implementation of the MultiMap2D class.
*/



namespace ag {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC MULTIMAP2D MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF MULTIMAP2D MEMBERS
//------------------------------------------------------------------------------

MultiMap2D::MultiMap2D(
         DataObject* dataObject,
         size_t nrRows,
         size_t nrCols,
         QWidget* parent)

  : Map(dataObject, "Multi 2D Map", parent),
    d_multiMap2DView(0), d_legendView(0)

{
  createInterface(nrRows, nrCols);
}



/* NOT IMPLEMENTED
//! Copy constructor.
MultiMap2D::MultiMap2D(MultiMap2D const& rhs)

  : Base(rhs)

{
}
*/



MultiMap2D::~MultiMap2D()
{
}



/* NOT IMPLEMENTED
//! Assignment operator.
MultiMap2D& MultiMap2D::operator=(MultiMap2D const& rhs)
{
  if (this != &rhs) {
  }
  return *this;
}
*/



void MultiMap2D::createInterface(
         size_t nrRows,
         size_t nrCols)
{
  d_splitter = new QSplitter(Qt::Horizontal, this);
  // d_splitter->setPalette(qt::LIGHTONDARKPALETTE);
  d_legendView = new LegendView(&dataObject(), VT_Map, d_splitter);
  d_multiMap2DView = new MultiMap2DView(&dataObject(), nrRows, nrCols, d_splitter);
  // d_splitter->moveToLast(d_multiMap2DView);
  d_splitter->setStretchFactor(d_splitter->indexOf(d_legendView), 0);
  d_splitter->setStretchFactor(d_splitter->indexOf(d_multiMap2DView), 1);
  d_splitter->setHandleWidth(5);

  QList<int> sizes;
  sizes.append(100);
  sizes.append(400);
  d_splitter->setSizes(sizes);

  QVBoxLayout* layout = new QVBoxLayout(this);
  layout->addWidget(d_splitter);
}



void MultiMap2D::addAttribute(DataGuide const& guide)
{
  d_legendView->addAttribute(guide);
  d_multiMap2DView->addAttribute(guide);
}



void MultiMap2D::addAttribute(size_t row, size_t col,
         DataGuide const& guide)
{
  d_legendView->addAttribute(guide);
  d_multiMap2DView->addAttribute(row, col, guide);
}



size_t MultiMap2D::nrCols() const
{
  return d_multiMap2DView->nrCols();
}



/*
void MultiMap2D::zoomAll()
{
  d_multiMap2DView->zoomAll();
}
*/



void MultiMap2D::resetMapView()
{
  d_multiMap2DView->resetMapView();
}



void MultiMap2D::saveAsPNG(
         boost::filesystem::path const& /* path */) const
{
  // QPixmap pixmap(d_map->pixmap());
}



void MultiMap2D::rescan()
{
  visualisationEngine().rescan(dataObject());
}



void MultiMap2D::process()
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



void MultiMap2D::visualise()
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


