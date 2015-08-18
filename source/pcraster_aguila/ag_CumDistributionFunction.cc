#include "ag_CumDistributionFunction.h"

// Library headers.
#include <QLayout>
#include <QSplitter>

// PCRaster library headers.

// Module headers.
#include "ag_CumDistributionFunctionView.h"
#include "ag_DataObject.h"
#include "ag_LegendView.h"
#include "ag_VisEngine.h"



/*!
  \file
  This file contains the implementation of the CumDistributionFunction class.
*/


namespace ag {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC CUMDISTRIBUTIONFUNCTION MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF CUMDISTRIBUTIONFUNCTION MEMBERS
//------------------------------------------------------------------------------

CumDistributionFunction::CumDistributionFunction(
         DataObject* object,
         QWidget* parent)

  : Visualisation<>(object, "Cumulative distribution function plot", parent),
    d_plotView(0), d_legendView(0)

{
  createInterface(object);
}



/* NOT IMPLEMENTED
//! Copy constructor.
CumDistributionFunction::CumDistributionFunction(CumDistributionFunction const& rhs)

  : Base(rhs)

{
}
*/



CumDistributionFunction::~CumDistributionFunction()
{
}



/* NOT IMPLEMENTED
//! Assignment operator.
CumDistributionFunction& CumDistributionFunction::operator=(CumDistributionFunction const& rhs)
{
  if (this != &rhs) {
  }
  return *this;
}
*/



void CumDistributionFunction::createInterface(DataObject* object)
{
  d_splitter = new QSplitter(Qt::Horizontal, this);
  QVBoxLayout* layout = new QVBoxLayout(this);
  layout->addWidget(d_splitter);

  d_legendView = new LegendView(object, VT_Graph, d_splitter);
  d_plotView = new CumDistributionFunctionView(object, d_splitter);

  // Set resize modes and initial sizes of the views.
  d_splitter->setStretchFactor(d_splitter->indexOf(d_legendView), 0);
  d_splitter->setStretchFactor(d_splitter->indexOf(d_plotView), 1);
  d_splitter->setHandleWidth(5);

  QList<int> sizes;
  sizes.append(100);
  sizes.append(400);
  d_splitter->setSizes(sizes);
}



void CumDistributionFunction::addAttribute(
         DataGuide const& guide)
{
  d_plotView->addAttribute(guide);
  d_legendView->addAttribute(guide);
}



QSize CumDistributionFunction::sizeHint() const
{
  return QSize(500, 300);
}



void CumDistributionFunction::toggleMarker()
{
  d_plotView->toggleMarker();
}



void CumDistributionFunction::rescan()
{
  visualisationEngine().rescan(dataObject());
}



void CumDistributionFunction::process()
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



void CumDistributionFunction::visualise()
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


