#include "ag_MultiMap2DWindow.h"

// Library headers.

// PCRaster library headers.

// Module headers.
#include "ag_DataObject.h"
#include "ag_MultiMap2D.h"
#include "ag_VisEngine.h"



/*!
  \file
  This file contains the implementation of the MultiMap2DWindow class.
*/



//------------------------------------------------------------------------------

namespace ag {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC MULTIMAP2DWINDOW MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF MULTIMAP2DWINDOW MEMBERS
//------------------------------------------------------------------------------

MultiMap2DWindow::MultiMap2DWindow(qt::AppWindowProperties const& props,
         DataObject* object, size_t nrRows, size_t nrCols)

  : MapWindow(props, "2D Multi Map", object),
    d_engines(nrRows * nrCols),
    d_map(0)

{
  for(size_t i = 0; i < d_engines.size(); ++i) {
    d_engines[i] = new VisEngine();
  }

  // std::vector<com::FileFormatInfo> fileFormats;
  // fileFormats.push_back(com::FileFormatInfo::png());
  // setSaveAsFileFormats(fileFormats);

  createInterface(nrRows, nrCols);
}



/* NOT IMPLEMENTED
//! Copy constructor.
MultiMap2DWindow::MultiMap2DWindow(MultiMap2DWindow const& rhs)

  : Base(rhs)

{
}
*/



MultiMap2DWindow::~MultiMap2DWindow()
{
  for(size_t i = 0; i < d_engines.size(); ++i) {
    delete d_engines[i];
  }
}



/* NOT IMPLEMENTED
//! Assignment operator.
MultiMap2DWindow& MultiMap2DWindow::operator=(MultiMap2DWindow const& rhs)
{
  if (this != &rhs) {
  }
  return *this;
}
*/



void MultiMap2DWindow::createInterface(size_t nrRows, size_t nrCols)
{
  MapWindow::createInterface();

  d_map = new MultiMap2D(&dataObject(), nrRows, nrCols, this);

  /// addMapActionGroup();
  addZoomAllAction(d_map);
  setCentralWidget(d_map);

  rescan();
}



void MultiMap2DWindow::rescan()
{
  for(size_t i = 0; i < d_engines.size(); ++i) {
    d_engines[i]->rescan(dataObject());
  }

  // visualisationEngine().rescan(dataObject());

  MapWindow::rescan();
}



void MultiMap2DWindow::addAttribute(DataGuide const& guide)
{
  d_map->addAttribute(guide);

  for(size_t i = 0; i < d_engines.size(); ++i) {
    d_engines[i]->addAttribute(dataObject(), guide);
  }

  visualisationEngine().addAttribute(dataObject(), guide);
}



void MultiMap2DWindow::addAttribute(size_t row, size_t col,
         DataGuide const& guide)
{
  d_map->addAttribute(row, col, guide);
  d_engines[row * d_map->nrCols() + col]->addAttribute(dataObject(), guide);
  visualisationEngine().addAttribute(dataObject(), guide);
}



std::string MultiMap2DWindow::windowName() const
{
  std::string name = "No data loaded";

  if(!d_engines.empty()) {
    std::vector<DataGuide> dataGuides = d_engines[0]->dataGuides();
    if(!dataGuides.empty()) {
      name = dataObject().name(dataGuides[0]);
      dataGuides = d_engines[0]->dataGuides();
      for(size_t i = 1; i < dataGuides.size(); ++i) {
        name += " + " + dataObject().name(dataGuides[i]);
      }

      for(size_t i = 1; i < d_engines.size(); ++i) {
        dataGuides = d_engines[i]->dataGuides();
        if(!dataGuides.empty()) {
          name += " | " + dataObject().name(dataGuides[0]);
          dataGuides = d_engines[i]->dataGuides();
          for(size_t j = 1; j < dataGuides.size(); ++j) {
            name += " + " + dataObject().name(dataGuides[j]);
          }
        }
      }
    }
  }


  /*
  std::vector<DataGuide> dataGuides = d_engine.dataGuides();

  if(!dataGuides.empty()) {
    name = dataObject().name(dataGuides[0]).baseName();
    for(size_t i = 1; i < dataGuides.size(); ++i) {
      name += " + " + dataObject().name(dataGuides[i]).baseName();
    }
  }
  */

  return name;
}



bool MultiMap2DWindow::dataVisualised() const
{
  return visualisationEngine().dataGuides().size() > 0;
}



void MultiMap2DWindow::saveAsPNG(
         boost::filesystem::path const& path)
{
  d_map->saveAsPNG(path);
}



// void MultiMap2DWindow::process()
// {
//   std::cout << "process" << std::endl;
//   MapWindow::process();
// }
// 
// 
// 
// void MultiMap2DWindow::visualise()
// {
//   MapWindow::visualise();
//   visualisationEngine().finishedScanning(dataObject());
// }



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace ag
