#include "ag_TimePlot.h"

// Library headers.
#include <QLayout>
#include <QSplitter>

// PCRaster library headers.
#include "qt_Const.h"

// Module headers.
#include "com_exception.h"
#include "ag_DataObject.h"
#include "ag_LegendView.h"
#include "ag_PlotView.h"
#include "ag_VisEngine.h"



/*!
  \file
  This file contains the implementation of the TimePlot class.
*/


namespace ag {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC TIMEPLOT MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF TIMEPLOT MEMBERS
//------------------------------------------------------------------------------

TimePlot::TimePlot(
         DataObject* object,
         QWidget* parent)

  : Visualisation<>(object, "Time plot", parent),
    _plotView(0), _legendView(0)

{
  createInterface(object);
}



TimePlot::~TimePlot()
{
}



void TimePlot::createInterface(DataObject* object)
{
  _splitter = new QSplitter(Qt::Horizontal, this);
  QVBoxLayout* layout = new QVBoxLayout(this);
  layout->addWidget(_splitter);

  _legendView = new LegendView(object, VT_Graph, _splitter);
  _plotView = new PlotView(object, _splitter);

  // Set resize modes and initial sizes of the views.
  _splitter->setStretchFactor(_splitter->indexOf(_legendView), 0);
  _splitter->setStretchFactor(_splitter->indexOf(_plotView), 1);
  _splitter->setHandleWidth(5);

  QList<int> sizes;
  sizes.append(100);
  sizes.append(400);
  _splitter->setSizes(sizes);
}



void TimePlot::addAttribute(const DataGuide& dataGuide)
{
  _plotView->addAttribute(dataGuide);
  _legendView->addAttribute(dataGuide);
}



QSize TimePlot::sizeHint() const
{
  return QSize(500, 300);
}



void TimePlot::saveAsPNG(
         boost::filesystem::path const& path) const
{
  QPixmap pixmap(_plotView->pixmap());
  if(pixmap.isNull()) {
    throw com::FileError(path.string(), "Error while saving");
  }

  if(!pixmap.save(QString(path.string().c_str()), "PNG")) {
    throw com::FileError(path.string(), "Error while saving");
  }
}



// void TimePlot::saveAsEPS(
//          com::PathName const& filename) const
// {
//   QPrinter printer;
//   printer.setOutputToFile(true);
//   printer.setOutputFileName(...);
//   ....
//   plot->print(&printer);
// }



void TimePlot::rescan()
{
  visualisationEngine().rescan(dataObject());
}



void TimePlot::process()
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



void TimePlot::visualise()
{
  // Done scanning, update stuff if needed.
  if(visualisationEngine().change() & VisEngine::BACKGROUND_COLOUR) {
    _splitter->update();
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

