#include "ag_CursorWindow.h"

// Library headers.
#include <sstream>
#include <boost/filesystem/fstream.hpp>
#include <boost/lexical_cast.hpp>
#include <QBoxLayout>
#include <QPushButton>

// PCRaster library headers.
#include "qt_AppWindow.h"
#include "pcrxsd_dominput.h"
#include "AguilaXSD.h"
#include "dal_DataSpaceAddressMapper.h"
#include "dal_FilesystemUtils.h"
#include "qt_Const.h"

// Module headers.
#include "ag_CursorView.h"
#include "ag_DataObject.h"
#include "ag_VisEngine.h"



/*!
  \file
  This file contains the implementation of the CursorWindow class.
*/



namespace ag {



//------------------------------------------------------------------------------
// DEFINITION OF STATIC CURSORWINDOW MEMBERS
//------------------------------------------------------------------------------

CursorWindow* CursorWindow::instance(DataObject* object)
{
  CursorWindow* dialog =
         VisualisationDialog<DataObject*, CursorWindow>::instance(
         object, object);

  if(dialog) {
    dialog->raise();
  }
  else {
    // Create and add instance.
    dialog = new CursorWindow(object);
    addInstance(object, object, dialog);
  }

  assert(dialog);

  return dialog;
}



//------------------------------------------------------------------------------
// DEFINITION OF CURSORWINDOW MEMBERS
//------------------------------------------------------------------------------

CursorWindow::CursorWindow(
         DataObject* object)

  : VisualisationDialog<DataObject*, CursorWindow>(object, "Cursor And Values"),
         // 0, false, Qt::WindowStaysOnTopHint),
    d_cursorView(0),
    d_save(0),
    d_get(0)

{
  createInterface();
}



CursorWindow::~CursorWindow()
{
}



void CursorWindow::createInterface()
{
  assert(!d_cursorView);

  d_cursorView = new CursorView(&dataObject(), this);
  QBoxLayout* topLayout = new QVBoxLayout(this);
  topLayout->addWidget(d_cursorView);

  QBoxLayout* buttonLayout = new QHBoxLayout();
  topLayout->addLayout(buttonLayout);

  // Stretch.
  buttonLayout->addStretch(1);

  // Button.
  QPushButton* close = new QPushButton("Close", this);
  close->setDefault(true);
  // close->setFixedSize(qt::BUTTONWIDTH, qt::BUTTONHEIGHT);
  connect(close, SIGNAL(clicked()), SLOT(close()));
  buttonLayout->addWidget(close);

  // Button.
  d_save = new QPushButton("Save", this);
  connect(d_save, SIGNAL(clicked()), SLOT(save()));
  d_save->setEnabled(false);
  buttonLayout->addWidget(d_save);

  // Button.
  d_get = new QPushButton("Get", this);
  d_get->setEnabled(false);
  connect(d_get, SIGNAL(clicked()), SLOT(get()));
  buttonLayout->addWidget(d_get);

  // Stretch.
  buttonLayout->addStretch(1);

  assert(d_cursorView);
}


void CursorWindow::get()
{
  pcrxsd::DOMInput d(pcrxsd::DOMInput::CompiledIn);
  d.setValidate(true);
  d.setFile(d_fileToGetCursorValue.string().c_str());

  try {
    std::unique_ptr<pcrxml::Cursor> cursor(pcrxml::aguilaCursor(*d.document()));

    if(cursor->x().present() && cursor->y().present()) {
      dataObject().setXY(cursor->x().get(), cursor->y().get());
    }
  }
  catch(pcrxsd::Exception const& e) {
    qt::AppWindow::showError("Aguila", e.msg());
  }
}

void CursorWindow::save()
{
/*
  // Ask for a filename to use.
  QString fileName = QFileDialog::getSaveFileName(QString::null,
         "Cursor and Values (*.xml)", this, "save file dialog",
         "Save cursor and values as...");

  if(!fileName.isEmpty()) {
    boost::filesystem::path path(dal::addExtensionIfNeeded(
         std::string(fileName.ascii()), ".xml"));
    saveToXMLHack(path);
  }
*/
  try {
#ifdef DEBUG_DEVELOP
    // If the cursor is outside the map boundary the S*** hits
    // the fan, user is however not interested in that so then
    // we may ignore the exception, and no data is appended to
    // d_cursorValueMonitorPath file
       // bool mustFixSaveToXMLHack=true;
       // PRINT_VAR(mustFixSaveToXMLHack);
#endif
       appendToCursorValueMonitorFile();
  } catch(...) {
#ifdef DEBUG_DEVELOP
       // bool mustFixSaveToXMLHack=true;
       // PRINT_VAR(mustFixSaveToXMLHack);
#endif
  }
}



//! write cursor and values into free from text format
void CursorWindow::saveToText(
    boost::filesystem::path const& path)
{
  // Get the current values and cursor settings and write them out.
  DataObject const& dataObject(d_cursorView->dataObject());
  dal::DataSpace const& space(dataObject.dataSpace());
  dal::DataSpaceAddress const& address(dataObject.dataSpaceAddress());

  // collect info in the streams and compose later to single file
  std::stringstream dataSpaceStream, globalCursorStream, worldCursorStream;

  for(size_t i = 0; i < space.size(); ++i) {
    dal::Dimension const& dimension(space.dimension(i));
    assert(dimension.nrValues() > 0);

    switch(dimension.meaning()) {
      case dal::Scenarios: {
        dataSpaceStream << "scenarios = {";

        for(size_t j = 0; j < dimension.nrValues() - 1; ++j) {
          dataSpaceStream << dimension.value<std::string>(j) << ", ";
        }

        dataSpaceStream
            << dimension.value<std::string>(dimension.nrValues() - 1)
            << "}\n";
        globalCursorStream << "scenario = *";
        worldCursorStream << "scenario = *";
        break;
      }
      case dal::CumulativeProbabilities: {
        dataSpaceStream << "cumulative probabilities = ";

        switch(dimension.discretisation()) {
          case dal::RegularDiscretisation: {
            dataSpaceStream << '['
                 << dimension.value<float>(0) << ", "
                 << dimension.value<float>(1) << ", "
                 << dimension.value<float>(2) << "]\n";
            break;
          }
          case dal::ExactDiscretisation: {
            dataSpaceStream << '{';

            for(size_t j = 0; j < dimension.nrValues() - 1; ++j) {
              dataSpaceStream << dimension.value<float>(j) << ", ";
            }

            dataSpaceStream
                 << dimension.value<float>(dimension.nrValues() - 1)
                 << "}\n";
            break;
          }
          default: {
            assert(false);
            break;
          }
        }

        globalCursorStream << "cummulative probability = "
            << address.coordinate<float>(i);
        worldCursorStream << "cummulative probability = "
            << dataObject.globalToWorldMapper().toString(address, i) << '\n';

        break;
      }
      case dal::Samples: {
        dataSpaceStream << "samples = ";

        switch(dimension.discretisation()) {
          case dal::RegularDiscretisation: {
            dataSpaceStream << '['
                 << dimension.value<size_t>(0) << ", "
                 << dimension.value<size_t>(1) << ", "
                 << dimension.value<size_t>(2) << "]\n";
            break;
          }
          case dal::ExactDiscretisation: {
            dataSpaceStream << '{';

            for(size_t j = 0; j < dimension.nrValues() - 1; ++j) {
              dataSpaceStream << dimension.value<size_t>(j) << ", ";
            }

            dataSpaceStream
                 << dimension.value<size_t>(dimension.nrValues() - 1)
                 << "}\n";
            break;
          }
          default: {
            assert(false);
            break;
          }
        }

        globalCursorStream << "sample = " << address.coordinate<size_t>(i);
        worldCursorStream << "sample = "
            << dataObject.globalToWorldMapper().toString(address, i) << '\n';

        break;
      }
      case dal::Time: {
        dataSpaceStream << "time = ";

        switch(dimension.discretisation()) {
          case dal::RegularDiscretisation: {
            dataSpaceStream << '['
                 << dimension.value<size_t>(0) << ", "
                 << dimension.value<size_t>(1) << ", "
                 << dimension.value<size_t>(2) << "]\n";
            break;
          }
          case dal::ExactDiscretisation: {
            dataSpaceStream << "{";

            for(size_t j = 0; j < dimension.nrValues() - 1; ++j) {
              dataSpaceStream << dimension.value<size_t>(j) << ", ";
            }

            dataSpaceStream
                 << dimension.value<size_t>(dimension.nrValues() - 1)
                 << "}\n";
            break;
          }
          default: {
            assert(false);
            break;
          }
        }

        // timestep
        globalCursorStream << "time = " << address.coordinate<size_t>(i);
        // real time
        worldCursorStream << "date = "
            << dataObject.globalToWorldMapper().toString(address, i) << '\n';

        break;
      }
      case dal::Space: {
        dal::SpatialCoordinate const& spatialAddress(
              address.coordinate<dal::SpatialCoordinate>(i));

        switch(dimension.discretisation()) {
          case dal::RegularDiscretisation: {
            dal::RasterDimensions const& rasterDimensions(
                   space.dimension(i).value<dal::RasterDimensions>(0));
            double row, col;
            rasterDimensions.indices(spatialAddress, row, col);

            /// dataSpaceStream << "rows = [1, 1, "
            ///        << rasterDimensions.nrRows() << "]\n";
            /// dataSpaceStream << "cols = [1, 1, "
            ///        << rasterDimensions.nrCols() << "]\n";

            globalCursorStream << "row = " << static_cast<size_t>(row) << '\n';
            globalCursorStream << "col = " << static_cast<size_t>(col) << '\n';

            break;
          }
          case dal::BorderedDiscretisation: {
            break;
          }
          default: {
            assert(false);
            break;
          }
        }

        worldCursorStream << "x = " << spatialAddress.x() << '\n';
        worldCursorStream << "y = " << spatialAddress.y() << '\n';

        // worldCursorStream
        //     << dataObject.globalToWorldMapper().toString(address, i) << '\n';

        break;
      }
      default: {
        assert(false);
        break;
      }
    }
  }

  dal::testPathIsWritable(path);
  boost::filesystem::ofstream stream(path);
  stream << "data space:\n" << dataSpaceStream.str();
  stream << "cursor position:\n" << globalCursorStream.str();
  stream << "cursor position in world coordinates:\n"
       << worldCursorStream.str();
  stream << "data sets:\n";

  std::vector<DataGuide> guides(dataObject.dataGuides());

  for(size_t i = 0; i < guides.size(); ++i) {
    stream
       << dataObject.name(guides[i]) << " = "
       << dataObject.label(guides[i]) << std::endl;
  }
}



//! append cursor and values to the specified d_cursorValueMonitorPath file
/*!
 * writes subset of what is written in saveToText
 */
void CursorWindow::appendToCursorValueMonitorFile()
{
 /* TODO: this is a rewrite  of saveToText, some info
  *  is still written to unused streams should be replaced
  *  with XML when schema of aguilaCursorValue is extended
  */

  // Get the current values and cursor settings and write them out.
  DataObject const& dataObject(d_cursorView->dataObject());
  dal::DataSpace const& space(dataObject.dataSpace());
  dal::DataSpaceAddress const& address(dataObject.dataSpaceAddress());

  // collect info in the streams, not used, see above
  std::stringstream dataSpaceStream, globalCursorStream, worldCursorStream;
  pcrxml::AguilaCursorValue acv;
  acv.cursor(pcrxml::Cursor());

  for(size_t i = 0; i < space.size(); ++i) {
    dal::Dimension const& dimension(space.dimension(i));
    assert(dimension.nrValues() > 0);

    switch(dimension.meaning()) {
      case dal::Scenarios: {
        dataSpaceStream << "scenarios = {";

        for(size_t j = 0; j < dimension.nrValues() - 1; ++j) {
          dataSpaceStream << dimension.value<std::string>(j) << ", ";
        }

        dataSpaceStream
            << dimension.value<std::string>(dimension.nrValues() - 1)
            << "}\n";
        globalCursorStream << "scenario = *";
        worldCursorStream << "scenario = *";
        break;
      }
      case dal::CumulativeProbabilities: {
        dataSpaceStream << "cumulative probabilities = ";

        switch(dimension.discretisation()) {
          case dal::RegularDiscretisation: {
            dataSpaceStream << '['
                 << dimension.value<float>(0) << ", "
                 << dimension.value<float>(1) << ", "
                 << dimension.value<float>(2) << "]\n";
            break;
          }
          case dal::ExactDiscretisation: {
            dataSpaceStream << '{';

            for(size_t j = 0; j < dimension.nrValues() - 1; ++j) {
              dataSpaceStream << dimension.value<float>(j) << ", ";
            }

            dataSpaceStream
                 << dimension.value<float>(dimension.nrValues() - 1)
                 << "}\n";
            break;
          }
          default: {
            assert(false);
            break;
          }
        }

        globalCursorStream << "cummulative probability = "
            << address.coordinate<float>(i);
        worldCursorStream << "cummulative probability = "
            << dataObject.globalToWorldMapper().toString(address, i) << '\n';

        break;
      }
      case dal::Samples: {
        dataSpaceStream << "samples = ";

        switch(dimension.discretisation()) {
          case dal::RegularDiscretisation: {
            dataSpaceStream << '['
                 << dimension.value<size_t>(0) << ", "
                 << dimension.value<size_t>(1) << ", "
                 << dimension.value<size_t>(2) << "]\n";
            break;
          }
          case dal::ExactDiscretisation: {
            dataSpaceStream << '{';

            for(size_t j = 0; j < dimension.nrValues() - 1; ++j) {
              dataSpaceStream << dimension.value<size_t>(j) << ", ";
            }

            dataSpaceStream
                 << dimension.value<size_t>(dimension.nrValues() - 1)
                 << "}\n";
            break;
          }
          default: {
            assert(false);
            break;
          }
        }

        globalCursorStream << "sample = " << address.coordinate<size_t>(i);
        worldCursorStream << "sample = "
            << dataObject.globalToWorldMapper().toString(address, i) << '\n';

        break;
      }
      case dal::Time: {
        dataSpaceStream << "time = ";

        switch(dimension.discretisation()) {
          case dal::RegularDiscretisation: {
            dataSpaceStream << '['
                 << dimension.value<size_t>(0) << ", "
                 << dimension.value<size_t>(1) << ", "
                 << dimension.value<size_t>(2) << "]\n";
            break;
          }
          case dal::ExactDiscretisation: {
            dataSpaceStream << "{";

            for(size_t j = 0; j < dimension.nrValues() - 1; ++j) {
              dataSpaceStream << dimension.value<size_t>(j) << ", ";
            }

            dataSpaceStream
                 << dimension.value<size_t>(dimension.nrValues() - 1)
                 << "}\n";
            break;
          }
          default: {
            assert(false);
            break;
          }
        }

        // timestep
        acv.cursor()->time(address.coordinate<size_t>(i));

        // real time
        std::string rt(dataObject.globalToWorldMapper().toString(address, i));
        // if not iso format then there is no real mapper
        // iso format has T seperator for date and time part
        if(rt.find("T") != std::string::npos) {
          // TODO XSD acv.cursor()->date(rt);
        }

        break;
      }
      case dal::Space: {
        assert(dimension.discretisation() ==
            dal::RegularDiscretisation);

        std::string worldStr =
           dataObject.globalToWorldMapper().toString(address, i);
        double v = boost::lexical_cast<double>(worldStr);
        if(i == space.indexOf(dal::Space)) {
          // First space dimension: rows, y coordinates.
          dataSpaceStream << "rows = ";
          globalCursorStream << "row = ";
          acv.cursor()->y(v);
        }
        else {
          // Second space dimension: cols, x coordinates.
          dataSpaceStream << "cols = ";
          globalCursorStream << "col = ";
          acv.cursor()->x(v);
        }

        dataSpaceStream << '['
            << dimension.value<size_t>(0) << ", "
            << dimension.value<size_t>(1) << ", "
            << dimension.value<size_t>(2) << "]\n";
        globalCursorStream << address.coordinate<size_t>(i) << '\n';

        break;
      }
      default: {
        assert(false);
        break;
      }
    }
  }

  std::vector<DataGuide> guides(dataObject.dataGuides());

  for(size_t i = 0; i < guides.size(); ++i) {
    acv.dataValue().push_back(
        pcrxml::DataValue(
         dataObject.name(guides[i]),
        dataObject.label(guides[i])));
  }

  pcrxsd::DOMInput d(pcrxsd::DOMInput::CompiledIn);
  d.setValidate(true);
  d.setFile(d_cursorValueMonitorPath.string().c_str());

  try {
    // append acv to existing file (created in setCursorIO)
    std::unique_ptr<pcrxml::AguilaCursorValues>
      appendToThis(pcrxml::aguilaCursorValues(*d.document()));

    appendToThis->aguilaCursorValue().push_back(acv);

    boost::filesystem::ofstream out(d_cursorValueMonitorPath);
    pcrxml::aguilaCursorValues(out,*appendToThis,
         pcrxsd::namespaceInfoMap("Aguila.xsd"));
  }
  catch(pcrxsd::Exception const& e) {
    qt::AppWindow::showError("Aguila", e.msg());
  }


  /*
  stream << "data space:\n" << dataSpaceStream.str();
  stream << "cursor position:\n" << globalCursorStream.str();
  stream << "cursor position in world coordinates:\n"
       << worldCursorStream.str();
  stream << "data sets:\n";
  */
}



/*
void CursorWindow::rescan()
{
  d_engine.rescan(dataObject());
  Visualisation::rescan();
}



std::string CursorWindow::windowName() const
{
  dal::DataSpace space(dataObject().dataSpace());
  dal::DataSpaceAddress address(dataObject().dataSpaceAddress());
  size_t index = space.indexOf(dal::Scenarios);

  if(index != space.rank()) {
    space.eraseDimension(index);
    address.eraseCoordinate(index);
  }

  return "Cursor: " + dal::dataSpaceAddressToString(space, address);
}
*/



void CursorWindow::setCursorIO(
     std::string const& cursorValueMonitorFile,
     std::string const& fileToGetCursorValue)
{
  if (!cursorValueMonitorFile.empty()) {
   d_cursorValueMonitorPath = boost::filesystem::path(
         dal::addExtensionIfNeeded(cursorValueMonitorFile, ".xml"));
   d_save->setEnabled(true);

   // create file with 0 sub-elements
   pcrxml::AguilaCursorValues acv;
   boost::filesystem::ofstream stream(d_cursorValueMonitorPath);
   pcrxml::aguilaCursorValues(stream,acv,pcrxsd::namespaceInfoMap("Aguila.xsd"));
  }
  if (!fileToGetCursorValue.empty()) {
   d_fileToGetCursorValue = boost::filesystem::path(
         dal::addExtensionIfNeeded(fileToGetCursorValue, ".xml"));
   d_get->setEnabled(true);
  }
}



void CursorWindow::rescan()
{
  visualisationEngine().rescan(dataObject());
}



void CursorWindow::process()
{
}



void CursorWindow::visualise()
{
  // was used to save each change to d_cursorValueMonitorPath
  // if(visualisationEngine().change() & VisEngine::CURSOR) {
  //   if(cursorValueMonitorFileMustBeCreated()) {
  //     saveToXMLHack(d_cursorValueMonitorPath);
  //   }
  // }

  visualisationEngine().finishedScanning(dataObject());
}



void CursorWindow::addAttribute(
         DataGuide const& guide)
{
  d_cursorView->addAttribute(guide);
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace ag

