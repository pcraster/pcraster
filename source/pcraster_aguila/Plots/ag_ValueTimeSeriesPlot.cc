#ifndef INCLUDED_AG_VALUETIMESERIESPLOT
#include "ag_ValueTimeSeriesPlot.h"
#define INCLUDED_AG_VALUETIMESERIESPLOT
#endif

// External headers.
#ifndef INCLUDED_BOOST_SCOPED_ARRAY
#include <boost/scoped_array.hpp>
#define INCLUDED_BOOST_SCOPED_ARRAY
#endif

#ifndef INCLUDED_BOOST_SCOPED_PTR
#include <boost/scoped_ptr.hpp>
#define INCLUDED_BOOST_SCOPED_PTR
#endif

#ifndef INCLUDED_QAPPLICATION
#include <QApplication>
#define INCLUDED_QAPPLICATION
#endif

#ifndef INCLUDED_QPEN
#include <QPen>
#define INCLUDED_QPEN
#endif

#ifndef INCLUDED_QWT_PLOT_CANVAS
#include <qwt_plot_canvas.h>
#define INCLUDED_QWT_PLOT_CANVAS
#endif

// Project headers.
#ifndef INCLUDED_DAL_MATHUTILS
#include "dal_MathUtils.h"
#define INCLUDED_DAL_MATHUTILS
#endif

#ifndef INCLUDED_DAL_TABLE
#include "dal_Table.h"
#define INCLUDED_DAL_TABLE
#endif

// Module headers.
#ifndef INCLUDED_QT_ANIMATION
#include "qt_Animation.h"
#define INCLUDED_QT_ANIMATION
#endif

#ifndef INCLUDED_AG_DATAOBJECT
#include "ag_DataObject.h"
#define INCLUDED_AG_DATAOBJECT
#endif

#ifndef INCLUDED_AG_VISENGINE
#include "ag_VisEngine.h"
#define INCLUDED_AG_VISENGINE
#endif



/*!
  \file
  This file contains the implementation of the ValueTimeSeriesPlot class.
*/

namespace {

} // Anonymous namespace



namespace ag {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC VALUETIMESERIESPLOT MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF VALUETIMESERIESPLOT MEMBERS
//------------------------------------------------------------------------------

ValueTimeSeriesPlot::ValueTimeSeriesPlot(
         DataObject* object,
         QWidget* parent)

  : PlotVisualisation(object, "Value Time Series Plot", parent),
    TimeSeriesPlot()

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

  QwtText title;
  title.setFont(QApplication::font());

  title.setText(QString("time step"));
  setAxisTitle(xBottom, title);

  title.setText(QString("value"));
  setAxisTitle(yLeft, title);

  enableMarker(xMarker());

  trackDragPoint();

  canvas()->setCursor(Qt::PointingHandCursor);
}



ValueTimeSeriesPlot::~ValueTimeSeriesPlot()
{
}



void ValueTimeSeriesPlot::rescan()
{
  visualisationEngine().rescan(dataObject());
}



bool ValueTimeSeriesPlot::recreatePlotRequired() const
{
  return visualisationEngine().change() & VisEngine::OTHERATTRIB ||
         visualisationEngine().change() & VisEngine::RASTER_CELL ||
         visualisationEngine().change() & VisEngine::DRAWPROPS ||
         visualisationEngine().change() & VisEngine::VALUE_SELECTION ||
         visualisationEngine().change() & VisEngine::SELECTION ||
         visualisationEngine().change() & VisEngine::QUANTILE;
}



bool ValueTimeSeriesPlot::replotRequired() const
{
  return recreatePlotRequired() ||
         visualisationEngine().change() & VisEngine::TIME ||
         visualisationEngine().change() & VisEngine::BACKGROUND_COLOUR;
}



void ValueTimeSeriesPlot::process()
{
  if(recreatePlotRequired()) {
    clearPlot();
    createPlot(visualisationEngine().dataGuides());
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



void ValueTimeSeriesPlot::visualise()
{
  if(replotRequired()) {
    replot();
  }

  visualisationEngine().finishedScanning(dataObject());
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
void ValueTimeSeriesPlot::createPlot(
         std::vector<DataGuide> const& dataGuides)
{
  if(dataGuides.empty()) {
    return;
  }

  if(!dataObject().dataSpace().hasTime()) {
    return;
  }

  DataObject const& object(dataObject());
  dal::DataSpace const& space(object.dataSpace());
  dal::DataSpaceAddress const& address(object.dataSpaceAddress());

  // Configure x-axis.--------------------------------------------------------
  size_t indexOfTime = space.indexOf(dal::Time);
  dal::Dimension const& dimension(space.dimension(indexOfTime));
  size_t first = dimension.value<size_t>(0);
  size_t last = dimension.value<size_t>(1);
  assert(last >= first);
  setAxisScale(xBottom, double(first), double(last));

  assert(address.isValid(indexOfTime));

  // Configure y-axis. -------------------------------------------------------
  double min, max;
  bool extremesInitialised = false;

  for(size_t i = 0; i < visualisationEngine().size(); ++i) {
    DataGuide const& guide = visualisationEngine().guide(i);
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
    }
  }

  setAxisScale(yLeft, min, max);

  // Draw plots. -------------------------------------------------------------
  for(size_t i = 0; i < visualisationEngine().size(); ++i) {
    DataGuide const& guide = visualisationEngine().guide(i);
    assert(guide.valueScale() == VS_SCALAR);
    boost::scoped_ptr<dal::Table> scopedTable;
    dal::Table const* table = 0;
    size_t timeColIndex, attrColIndex;

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
      }
      default : {
        assert(false);
        break;
      }
    }

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

    QPen pen(dataObject().properties().colour(guide),
         dataObject().isSelected(guide) ? 2 : 1, Qt::SolidLine);
    drawCurve(guide, x.get(), y.get(), table->nrRecs(), pen);
  }

  attachMarkers();
}



void ValueTimeSeriesPlot::addAttribute(
         DataGuide const& dataGuide)
{
  assert(dataObject().dataSpace(dataGuide).hasTime());

  testDataGuide(dataGuide);
  visualisationEngine().addAttribute(dataObject(), dataGuide);
}



void ValueTimeSeriesPlot::appended(
         QwtDoublePoint const& point)
{
  moved(point);
}



void ValueTimeSeriesPlot::moved(
         QwtDoublePoint const& point)
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



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace ag

