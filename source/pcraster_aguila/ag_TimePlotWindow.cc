#include "ag_TimePlotWindow.h"
#include <boost/format.hpp>
#include "com_exception.h"
#include "ag_DataObject.h"
#include "ag_TimePlot.h"
#include "ag_VisEngine.h"



//------------------------------------------------------------------------------
// DEFINITION OF STATIC CLASS MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF CLASS MEMBERS 
//------------------------------------------------------------------------------

ag::TimePlotWindow::TimePlotWindow(const qt::AppWindowProperties& props,
         DataObject* object)

  : VisualisationWindow(props, "Timeplot", object, Qt::Window)

{
  std::vector<com::FileFormatInfo> fileFormats;
  fileFormats.push_back(com::FileFormatInfo::png());
  setSaveAsFileFormats(fileFormats);

  createInterface();
}



ag::TimePlotWindow::~TimePlotWindow()
{
}



void ag::TimePlotWindow::createInterface()
{
  VisualisationWindow::createInterface();

  d_plot = new TimePlot(&dataObject(), this);

  setCentralWidget(d_plot);
}



void ag::TimePlotWindow::rescan()
{
  visualisationEngine().rescan(dataObject());
  VisualisationWindow::rescan();
}



void ag::TimePlotWindow::addAttribute(
         DataGuide const& guide)
{
  dal::DataSpace space(dataObject().dataSpace(guide));

  if(!space.hasTime()) {
    std::string message = (boost::format(
         "Unable to add %1% to a %2% visualisation.\n"
         "Dataset does not contain temporal information.\n"
         "Dimensions present: %3%.")
         % dataObject().description(guide)
         % visualisationName()
         % dal::dataSpaceToString(space)).str();
    throw com::Exception(message);
  }

  visualisationEngine().addAttribute(dataObject(), guide);
  d_plot->addAttribute(guide);
}



std::string ag::TimePlotWindow::windowName() const
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



bool ag::TimePlotWindow::dataVisualised() const
{
  return visualisationEngine().dataGuides().size() > 0;
}



void ag::TimePlotWindow::saveAsPNG(
         boost::filesystem::path const& path)
{
  d_plot->saveAsPNG(path);
}



/*
void ag::TimePlotWindow::showEvent(QShowEvent* event)
{
  if(!event->spontaneous()) {

    // We are shown on purpose. Since we have been sleeping untill now we must
    // rescan to find out what we should do.

    // If this is the first time the map view is configured with data, than
    // we have to set the scale of the view. Otherwise nothing will be shown.
    // if(d_data->d_mapView->scale() == 0.0) {
    //   zoomAll();
    // }

    rescan();
  }
}
*/



/*
ag::TimePlotWindow *ag::TimePlotWindow::copy(ag::DataObject *o) const
{
  // FIXME

  assert(false);

  TimePlotWindow *p = new TimePlotWindow(windowProperties(), o);
  p->resize(size());
  return p;
}
*/



/*!
 *  \todo
 *    does save the correct part of the tss
 */
 /*
void ag::TimePlotWindow::saveAsPNG(const com::PathName& pathName) 
{
  if(d_data->d_plotView->pixmap()->isNull()) {
    throw com::FileError(pathName, "Error while saving");
  }

  if(!d_data->d_plotView->pixmap()->save(QString(pathName.toString().c_str()),
                   "PNG")) {
    throw com::FileError(pathName, "Error while saving");
  }
}
*/

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


