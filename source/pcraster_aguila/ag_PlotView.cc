#include "ag_PlotView.h"

// Library headers.
#include <boost/scoped_array.hpp>
#include <boost/scoped_ptr.hpp>
#include <QApplication>
#include <QPen>
#include <qwt_plot_canvas.h>

// PCRaster library headers.
#include "dal_MathUtils.h"
#include "dal_Table.h"

// Module headers.
#include "qt_Animation.h"
#include "ag_DataObject.h"
#include "ag_DataProperties.h"
#include "ag_NominalDrawProps.h"
#include "ag_Raster.h"
#include "ag_RasterDataSources.h"
#include "ag_Table.h"
#include "ag_TableDataSources.h"
#include "ag_VisEngine.h"



/*!
  \file
  This file contains the implementation of the PlotView class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC PLOTVIEW MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF PLOTVIEW MEMBERS
//------------------------------------------------------------------------------

namespace ag {

PlotView::PlotView(DataObject* object,
         QWidget* parent, const char* name)

  : PlotVisualisation(object, "Time Series View", parent, name)

{
  // Supported data types.
  std::vector<geo::DataType> dataTypes;
  dataTypes.push_back(geo::STACK);
  dataTypes.push_back(geo::FEATURE);
  dataTypes.push_back(geo::VECTOR);
  dataTypes.push_back(geo::TIMESERIES);
  setSupportedDataTypes(dataTypes);

  std::vector<CSF_VS> valueScales;
  valueScales.push_back(VS_SCALAR);
  setSupportedValueScales(valueScales);

  // Supported file formats.
  // std::vector<com::FileFormatInfo> fileFormats;
  // fileFormats.push_back(com::FileFormatInfo::png());
  // setSaveAsFileFormats(fileFormats);

  enableMarker(xMarker());
  trackDragPoint();

  // setAxisFont(xBottom, QApplication::font());
  // setAxisFont(yLeft, QApplication::font());

  canvas()->setCursor(Qt::PointingHandCursor);
}



PlotView::~PlotView()
{
}



void PlotView::rescan()
{
  visualisationEngine().rescan(dataObject());
}



bool PlotView::recreatePlotRequired() const
{
  return visualisationEngine().change() & VisEngine::OTHERATTRIB ||
         visualisationEngine().change() & VisEngine::RASTER_CELL ||
         visualisationEngine().change() & VisEngine::DRAWPROPS ||
         visualisationEngine().change() & VisEngine::VALUE_SELECTION ||
         visualisationEngine().change() & VisEngine::SELECTION ||
         visualisationEngine().change() & VisEngine::QUANTILE;
}



bool PlotView::replotRequired() const
{
  return recreatePlotRequired() ||
         visualisationEngine().change() & VisEngine::TIME ||
         visualisationEngine().change() & VisEngine::BACKGROUND_COLOUR;
}



void PlotView::process()
{
  if(recreatePlotRequired()) {
    clearPlot();
    createPlot();
  }

  if(visualisationEngine().change() & VisEngine::TIME) {
    dal::DataSpace const& space(dataObject().dataSpace());

    if(space.hasTime()) {
      dal::DataSpaceAddress const& address(dataObject().dataSpaceAddress());
      size_t index = space.indexOf(dal::Time);
      double timeStep = address.coordinate<size_t>(index);

      setXMarker(timeStep);
    }
  }

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



void PlotView::visualise()
{
  if(replotRequired()) {
    replot();
  }

  visualisationEngine().finishedScanning(dataObject());
}



void PlotView::addAttribute(
         DataGuide const& dataGuide)
{
  assert(dataObject().dataSpace(dataGuide).hasTime());

  testDataGuide(dataGuide);
  visualisationEngine().addAttribute(dataObject(), dataGuide);
}



void PlotView::setXAxisTitle()
{
  QwtText title;

  title.setFont(QApplication::font());
  title.setText(QString("Time step"));
  setAxisTitle(xBottom, title);
}



void PlotView::setYAxisTitle()
{
  QwtText title;

  title.setFont(QApplication::font());

  if(!dataObject().hasSelectedValue()) {
    title.setText(QString("Value"));
  }
  else {
    if(onlyCumulativeProbabilitiesShown()) {
      title.setText(QString("Cumulative probability"));
    }
    else if(onlyExceedanceProbabilitiesShown()) {
      title.setText(QString("Exceedance probability"));
    }
    else {
      title.setText(QString("Probability"));
    }
  }

  setAxisTitle(yLeft, title);
}



void PlotView::setXAxisScale()
{
  dal::DataSpace const& space(dataObject().dataSpace());
  assert(space.hasTime());
  size_t indexOfTime = space.indexOf(dal::Time);
  dal::Dimension const& dimension(space.dimension(indexOfTime));
  size_t first = dimension.value<size_t>(0);
  size_t last = dimension.value<size_t>(1);
  assert(last >= first);

  setAxisScale(xBottom, double(first), double(last));
}



void PlotView::setYAxisScale()
{
  double min = 0.0;
  double max = 1.0;
  bool extremesInitialised = false;
  // bool classificationAlgorithmsAreEqual = true;
  // RangeDrawProps::Algorithm classificationAlgorithm = INVALID_ALGORITHM;

  BOOST_FOREACH(DataGuide const& guide, visualisationEngine().dataGuides()) {
    assert(guide.valueScale() == VS_SCALAR);

    RangeDrawProps const& properties(
         dataObject().properties().rangeDrawProperties(guide));

    if(properties.cutoffsAreValid()) {
      if(!extremesInitialised) {
        min = properties.minCutoff();
        max = properties.maxCutoff();
        extremesInitialised = true;
      }
      else {
        min = std::min(min, properties.minCutoff());
        max = std::max(max, properties.maxCutoff());
      }

      // if(classificationAlgorithm == INVALID_ALGORITHM) {
      //   assert(properties.algorithm() != INVALID_ALGORITHM);
      //   classificationAlgorithm = properties.algorithm();
      // }
      // else {
      //   if(properties.algorithm() != classificationAlgorithm) {
      //     classificationAlgorithmsAreEqual = false;
      //   }
      // }
    }
  }

  // if(!classificationAlgorithmsAreEqual) {
  //   setAxisScaleEngine(yLeft, QwtLinearScaleEngine());
  // }
  //   switch(classificationAlgorithm) {
  //     case LOG: {
  //       setAxisScaleEngine(yLeft, new QwtLog10ScaleEngine());
  //       break;
  //     }
  //     case TLOG: {
  //       setAxisScaleEngine(yLeft, new QwtTLog10ScaleEngine());
  //       break;
  //     }
  //     case USERDEFINED:
  //     case INVALID_ALGORITHM:
  //     case LIN: {
  //       setAxisScaleEngine(yLeft, QwtLinearScaleEngine());
  //       break;
  //     }
  //   }
  // }

  setAxisScale(yLeft, min, max);
}



void PlotView::configureXAxis()
{
  setXAxisTitle();
  setXAxisScale();
}



void PlotView::configureYAxis()
{
  setYAxisTitle();
  setYAxisScale();
}



void PlotView::drawPlots()
{
  boost::scoped_ptr<dal::Table> scopedTable;
  dal::Table const* table = 0; // Shut up compiler.
  size_t timeColIndex = 0; // Shut up compiler.
  size_t attrColIndex = 0; // Shut up compiler.

  BOOST_FOREACH(DataGuide const& guide, visualisationEngine().dataGuides()) {
    assert(guide.valueScale() == VS_SCALAR);

    // Set table, timeColIndex and attrColIndex.
    switch(guide.type()) {
      case geo::STACK: {
        // This table is generated so we keep it in a scoped pointer.
        scopedTable.reset(new dal::Table());
        dataObject().rasterDataSources().data(guide).readTimeSeries(
              dataObject().dataSpace(), dataObject().dataSpaceAddress(),
              *scopedTable.get());
        table = scopedTable.get();
        timeColIndex = 0;
        attrColIndex = 1;
        break;
      }
      case geo::FEATURE: {
        // This table is generated so we keep it in a scoped pointer.
        scopedTable.reset(new dal::Table());
        dataObject().featureDataSources().data(guide).readTimeSeries(
              dataObject().dataSpace(), dataObject().dataSpaceAddress(),
              *scopedTable.get());
        table = scopedTable.get();
        timeColIndex = 0;
        attrColIndex = 1;
        break;
      }
      case geo::VECTOR: {
        // This table is generated so we keep it in a scoped pointer.
        scopedTable.reset(new dal::Table());
        dataObject().vectorDataSources().data(guide).readTimeSeries(
              dataObject().dataSpace(), dataObject().dataSpaceAddress(),
              *scopedTable.get());
        table = scopedTable.get();
        timeColIndex = 0;
        attrColIndex = 1;
        break;
      }
      case geo::TIMESERIES: {
        // This table is for use only.
        Table const& table1 = dataObject().tableDataSources().data(guide);
        table = &table1.table();
        timeColIndex = table1.timeCol();
        attrColIndex = table1.attrCol();
        break;
      }
      default : {
        assert(false);
        break;
      }
    }

    assert(table);
    assert(timeColIndex < table->nrCols());
    assert(attrColIndex < table->nrCols());

    // Convert table values to double arrays, since that is what
    // drawCurve expects.
    dal::Array<UINT4> const& timeCol(table->col<UINT4>(timeColIndex));
    dal::Array<REAL4> const& attrCol(table->col<REAL4>(attrColIndex));
    boost::scoped_array<double> x(new double[table->nrRecs()]);
    boost::scoped_array<double> y(new double[table->nrRecs()]);

    for(size_t i = 0; i < table->nrRecs(); ++i) {
      x[i] = timeCol[i];

      if(pcr::isMV(attrCol[i])) {
        pcr::setMV(y[i]);
      }
      else {
        y[i] = attrCol[i];
      }
    }

    if(dataObject().dataSpace(guide).hasCumProbabilities()) {
      RangeDrawProps const& properties(
         dataObject().properties().rangeDrawProperties(guide));

      if(dataObject().hasSelectedValue() && properties.probabilityScale() ==
              RangeDrawProps::ExceedanceProbabilities) {
        // Exceedance probabilities are 1 - cumulative probabilities.
        for(size_t i = 0; i < table->nrRecs(); ++i) {
          if(!pcr::isMV(y[i])) {
            y[i] = 1.0 - y[i];
          }
        }
      }
    }

    QPen pen(dataObject().properties().colour(guide),
         dataObject().isSelected(guide) ? 2 : 1, Qt::SolidLine);
    drawCurve(guide, x.get(), y.get(), table->nrRecs(), pen);
  }
}



//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
  \todo      Handle log scale setting, see QwtAutoScale, use this class
             everywhere were needed, in favour of our own version.
*/
void PlotView::createPlot()
{
  if(visualisationEngine().isEmpty()) {
    return;
  }

  if(!dataObject().dataSpace().hasTime()) {
    return;
  }

  configureXAxis();
  configureYAxis();
  drawPlots();
  attachMarkers();
}



// void PlotView::selected(QPointF const& /* point */)
// {
//   // Determine whether a marker is selected. If so deselect it.
//   long int marker = selectedMarker();
//   if(marker) {
//     unselectMarker(marker);
//   }
// }



void PlotView::appended(QPointF const& point)
{
  /*
  // Determine whether an active marker is nearby. If so, select marker.
  long int marker = selectedMarker(point);
  if(marker) {
    selectMarker(marker);
  }
  */
  moved(point);
}



void PlotView::moved(QPointF const& point)
{
  if(markerEnabled(xMarker())) {
    // Stop the animation if it is running. It is anoying to keep it running.
    // The user wants to select a certain date.
    dataObject().animationManager().stop();

    // Snap to closest time step.
    dal::DataSpace const& space = dataObject().dataSpace();
    if(space.hasTime()) {
      size_t index = space.indexOf(dal::Time);
      dal::Dimension const& dimension = space.dimension(index);

      // Prevent negative x-coordinates.
      dataObject().setTimeStep(dimension.clamp<size_t>(
         static_cast<size_t>(
              std::max<int>(0, dal::round<double, int>(point.x())))));
    }
  }
}

} // namespage ag

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



