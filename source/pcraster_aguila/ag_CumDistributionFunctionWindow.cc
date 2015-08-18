#include "ag_CumDistributionFunctionWindow.h"

// Library headers.
#include <QAction>
#include <QMenu>
#include <QToolBar>

// PCRaster library headers.

// Module headers.
#include "ag_CumDistributionFunction.h"
#include "ag_DataObject.h"
#include "ag_VisEngine.h"
#include "icons/togglemarker.xpm"



/*!
  \file
  This file contains the implementation of the CumDistributionFunctionWindow class.
*/



//------------------------------------------------------------------------------

/*
namespace ag {

class CumDistributionFunctionWindowPrivate
{
public:

  CumDistributionFunctionWindowPrivate()
  {
  }

  ~CumDistributionFunctionWindowPrivate()
  {
  }

};

} // namespace ag
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC CUMDISTRIBUTIONFUNCTIONWINDOW MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF CUMDISTRIBUTIONFUNCTIONWINDOW MEMBERS
//------------------------------------------------------------------------------

ag::CumDistributionFunctionWindow::CumDistributionFunctionWindow(
         qt::AppWindowProperties const& props, DataObject* object)

: VisualisationWindow(props, "Cumulative Distribution Function", object,
         Qt::Window),
  d_cumDistributionFunction(0)

{
  createInterface();
}



/* NOT IMPLEMENTED
//! Copy constructor.
ag::CumDistributionFunctionWindow::CumDistributionFunctionWindow(CumDistributionFunctionWindow const& rhs)

  : Base(rhs)

{
}
*/



ag::CumDistributionFunctionWindow::~CumDistributionFunctionWindow()
{
}



/* NOT IMPLEMENTED
//! Assignment operator.
ag::CumDistributionFunctionWindow& ag::CumDistributionFunctionWindow::operator=(CumDistributionFunctionWindow const& rhs)
{
  if (this != &rhs) {
  }
  return *this;
}
*/



void ag::CumDistributionFunctionWindow::createInterface()
{
  VisualisationWindow::createInterface();

  d_cumDistributionFunction = new CumDistributionFunction(&dataObject(), this);

  viewMenu()->addSeparator();
  toolBar()->addSeparator();

  QPixmap toggleMarkerIcon = QPixmap((const char**)togglemarker_xpm);
  QAction* action = new QAction(
         toggleMarkerIcon,
         "Toggle marker",
         this);
  connect(action, SIGNAL(triggered()),
         this, SLOT(toggleMarker()));
  addToMenuAndToolBar(viewMenu(), action, false);

  setCentralWidget(d_cumDistributionFunction);
}



void ag::CumDistributionFunctionWindow::addAttribute(
         DataGuide const& guide)
{
  visualisationEngine().addAttribute(dataObject(), guide);
  d_cumDistributionFunction->addAttribute(guide);
}



std::string ag::CumDistributionFunctionWindow::windowName() const
{
  std::string name = "No data loaded";
  std::vector<DataGuide> dataGuides = visualisationEngine().dataGuides();

  if(!dataGuides.empty()) {
    name = dataObject().name(dataGuides[0]);
    for(size_t i = 1; i < dataGuides.size(); ++i) {
      name += " + " + dataObject().name(dataGuides[i]);
    }
  }

  return name;
}



bool ag::CumDistributionFunctionWindow::dataVisualised() const
{
  return visualisationEngine().dataGuides().size() > 0;
}



void ag::CumDistributionFunctionWindow::toggleMarker()
{
  d_cumDistributionFunction->toggleMarker();
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



