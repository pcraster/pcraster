#include "ag_CumDistributionFunctionView.h"

// Library headers.
#include <boost/smart_ptr.hpp>
#include <QApplication>
#include <QPen>
#include <qwt_plot_canvas.h>
#include <qwt_scale_div.h>

// PCRaster library headers.
#include "dal_DataSpace.h"
#include "dal_Table.h"
#include "dal_MathUtils.h"
#include "com_userdefinedclassifier.h"

// Module headers.
#include "ag_DataObject.h"
#include "ag_DataProperties.h"
#include "ag_Raster.h"
#include "ag_RasterDataSources.h"
#include "ag_VisEngine.h"



/*!
  \file
  This file contains the implementation of the CumDistributionFunctionView class.
*/



namespace ag {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC CUMDISTRIBUTIONFUNCTIONVIEW MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF CUMDISTRIBUTIONFUNCTIONVIEW MEMBERS
//------------------------------------------------------------------------------

CumDistributionFunctionView::CumDistributionFunctionView(
         DataObject* object, QWidget* parent, const char* name)

  : PlotVisualisation(object, "Cumulative Distribution Function View",
         parent, name)

{
  // Supported data types.
  std::vector<geo::DataType> dataTypes;
  dataTypes.push_back(geo::STACK);
  dataTypes.push_back(geo::FEATURE);
  setSupportedDataTypes(dataTypes);

  std::vector<CSF_VS> valueScales;
  valueScales.push_back(VS_SCALAR);
  setSupportedValueScales(valueScales);

  trackDragPoint();

  canvas()->setCursor(Qt::PointingHandCursor);
}



CumDistributionFunctionView::~CumDistributionFunctionView()
{
}



void CumDistributionFunctionView::rescan()
{
  visualisationEngine().rescan(dataObject());
}



void CumDistributionFunctionView::process()
{
  if(visualisationEngine().change() & VisEngine::OTHERATTRIB ||
         visualisationEngine().change() & VisEngine::DRAWPROPS ||
         visualisationEngine().change() & VisEngine::SELECTION ||
         visualisationEngine().change() & VisEngine::TIME ||
         visualisationEngine().change() & VisEngine::RASTER_CELL) {
    clearPlot();
    createPlot();
  }
  else {
    if(visualisationEngine().change() & VisEngine::QUANTILE) {
      assert(!(visualisationEngine().change() & VisEngine::VALUE_SELECTION));

      dal::DataSpace const& space(dataObject().dataSpace());

      if(space.hasCumProbabilities()) {
        dal::DataSpaceAddress const& address(dataObject().dataSpaceAddress());
        size_t index = space.indexOf(dal::CumulativeProbabilities);
        double quantile = address.coordinate<float>(index);

        setYMarker(quantile);
      }
    }
    else if(visualisationEngine().change() & VisEngine::VALUE_SELECTION) {
      assert(!(visualisationEngine().change() & VisEngine::QUANTILE));
      assert(dataObject().hasSelectedValue());

      setXMarker(dataObject().selectedValue());
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



void CumDistributionFunctionView::visualise()
{
  if(visualisationEngine().change() & VisEngine::OTHERATTRIB ||
         visualisationEngine().change() & VisEngine::DRAWPROPS ||
         visualisationEngine().change() & VisEngine::VALUE_SELECTION ||
         visualisationEngine().change() & VisEngine::SELECTION ||
         visualisationEngine().change() & VisEngine::RASTER_CELL ||
         visualisationEngine().change() & VisEngine::TIME ||
         visualisationEngine().change() & VisEngine::QUANTILE ||
         visualisationEngine().change() & VisEngine::BACKGROUND_COLOUR) {
    replot();
  }

  visualisationEngine().finishedScanning(dataObject());
}



void CumDistributionFunctionView::addAttribute(
         DataGuide const& dataGuide) {
  testDataGuide(dataGuide);
  visualisationEngine().addAttribute(dataObject(), dataGuide);
}



void CumDistributionFunctionView::setXAxisTitle()
{
  QwtText title;

  title.setFont(QApplication::font());
  title.setText(QString("Value"));
  setAxisTitle(xBottom, title);
}



void CumDistributionFunctionView::setYAxisTitle()
{
  QwtText title;

  title.setFont(QApplication::font());

  if(onlyCumulativeProbabilitiesShown()) {
    title.setText(QString("Cumulative probability"));
  }
  else if(onlyExceedanceProbabilitiesShown()) {
    title.setText(QString("Exceedance probability"));
  }
  else {
    title.setText(QString("Probability"));
  }

  setAxisTitle(yLeft, title);
}



void CumDistributionFunctionView::setXAxisScale()
{
  double min, max;
  pcr::setMV(min);
  pcr::setMV(max);
  bool extremesInitialised = false;

  if(!dataObject().hasSelectedValue()) {
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
      }
    }
  }
  else {
    SpatialDataset* dataset;

    BOOST_FOREACH(DataGuide const& guide, visualisationEngine().dataGuides()) {
      assert(guide.type() == geo::STACK || guide.type() == geo::FEATURE);
      assert(guide.valueScale() == VS_SCALAR);
      dataset = 0;

      switch(guide.type()) {
        case geo::STACK: {
          dataset = &dataObject().rasterDataSources().data(guide);
          break;
        }
        case geo::FEATURE: {
          dataset = &dataObject().featureDataSources().data(guide);
          break;
        }
        default: {
          assert(false);
          break;
        }
      }

      assert(dataset);
      assert(dataset->dataSpace().hasCumProbabilities());

      if(!dataset->allMV()) {
        if(!extremesInitialised) {
          min = dataset->min<REAL4>();
          max = dataset->max<REAL4>();
          extremesInitialised = true;
        }
        else {
          min = std::min(min, double(dataset->min<REAL4>()));
          max = std::max(max, double(dataset->max<REAL4>()));
        }
      }
    }
  }

  if(!pcr::isMV(min) && !pcr::isMV(max)) {
    assert(min <= max);
    setAxisScale(xBottom, min, max);
  }
}



void CumDistributionFunctionView::setYAxisScale()
{
  double min = 0.0;
  double max = 1.0;

  if(dataObject().hasSelectedValue()) {
    bool extremesInitialised = false;

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
      }
    }
  }

  setAxisScale(yLeft, min, max);
}



void CumDistributionFunctionView::configureXAxis()
{
  setXAxisTitle();
  setXAxisScale();
}



void CumDistributionFunctionView::configureYAxis()
{
  setYAxisTitle();
  setYAxisScale();
}



void CumDistributionFunctionView::drawPlots()
{
  DataObject& object(dataObject());
  dal::DataSpace const& space(object.dataSpace());
  dal::DataSpaceAddress const& address(dataObject().dataSpaceAddress());

  SpatialDataset* dataset;

  BOOST_FOREACH(DataGuide const& guide, visualisationEngine().dataGuides()) {
    assert(guide.type() == geo::STACK || guide.type() == geo::FEATURE);
    assert(guide.valueScale() == VS_SCALAR);

    dataset = 0;

    switch(guide.type()) {
      case geo::STACK: {
        dataset = &object.rasterDataSources().data(guide);
        break;
      }
      case geo::FEATURE: {
        dataset = &object.featureDataSources().data(guide);
        break;
      }
      default: {
        assert(false);
        break;
      }
    }

    assert(dataset);
    assert(dataset->dataSpace().hasCumProbabilities());

    if(!dataset->allMV()) {
      // Create table for data values at the quantile levels.
      dal::Table table;
      dataset->readCumulativeProbabilities(space, address, table);

      dal::Array<REAL4> const& quantileCol(table.col<REAL4>(0));
      dal::Array<REAL4> const& attrCol(table.col<REAL4>(1));
      boost::scoped_array<double> x(new double[table.nrRecs()]);
      boost::scoped_array<double> y(new double[table.nrRecs()]);

      RangeDrawProps const& properties(
        object.properties().rangeDrawProperties(guide));

      for(size_t i = 0; i < table.nrRecs(); ++i) {
        y[i] = quantileCol[i];

        if(properties.probabilityScale() ==
               RangeDrawProps::ExceedanceProbabilities) {
          y[i] = 1.0 - y[i];
        }

        if(pcr::isMV(attrCol[i])) {
          pcr::setMV(x[i]);
        }
        else {
          x[i] = attrCol[i];
        }
      }

      QPen pen;

      if(object.isSelected(guide)) {
        pen = QPen(object.properties().colour(guide), 2, Qt::SolidLine);
      }
      else {
        pen = QPen(object.properties().colour(guide), 1, Qt::SolidLine);
      }

      drawCurve(guide, x.get(), y.get(), table.nrRecs(), pen);
    }
  }

  if(object.hasSelectedValue(/* guide */)) {
    // A value is selected. Use the first guide for the properties of
    // the marker. Mark data values.
    setXMarker(object.selectedValue(/* guide */));
    enableMarker(xMarker());
    disableMarker(yMarker());
  }
  else {
    size_t indexOfCumProbabilities = space.indexOf(
         dal::CumulativeProbabilities);
    assert(address.isValid(indexOfCumProbabilities));
    float quantile = address.coordinate<float>(indexOfCumProbabilities);

    setYMarker(quantile);
    disableMarker(xMarker());
    enableMarker(yMarker());
  }
}



void CumDistributionFunctionView::createPlot()
{
  if(visualisationEngine().isEmpty()) {
    return;
  }

  if(!dataObject().dataSpace().hasCumProbabilities()) {
    return;
  }

  configureXAxis();
  configureYAxis();
  drawPlots();
  attachMarkers();
}



void CumDistributionFunctionView::appended(
         QPointF const& point)
{
  moved(point);
}



void CumDistributionFunctionView::moved(
         QPointF const& point)
{
  if(markerEnabled(xMarker())) {
    /// for(size_t i = 0; i < visualisationEngine().size(); ++i) {
    ///   DataGuide const& guide = visualisationEngine().guide(i);
    ///   // assert(guide.type() == geo::STACK);
    ///   assert(guide.valueScale() == VS_SCALAR);
    ///   dataObject().setSelectedValue<REAL4>(guide, static_cast<float>(point.x()), false);
    /// }

    dataObject().setSelectedValue(static_cast<REAL4>(point.x()), false);
    /// dataObject().notify();
  }
  else if(markerEnabled(yMarker())) {
    // Snap to closest quantile.
    dal::DataSpace const& space = dataObject().dataSpace();
    if(space.hasCumProbabilities()) {
      size_t index = space.indexOf(dal::CumulativeProbabilities);
      dal::Dimension const& dimension = space.dimension(index);
      dataObject().setQuantile(dimension.clamp<float>(static_cast<float>(point.y())), false);
    }
  }

  dataObject().notify();
}



QSize CumDistributionFunctionView::minimumSizeHint() const
{
  // Override QwtPlot one with the default.
  return QWidget::minimumSizeHint();
}



void CumDistributionFunctionView::toggleMarker()
{
  // Determine which marker is on: the one which iterates over the quantiles
  // or the one which iterates over the data values.
  // Then switch from the one marker to the other and adjust relevant settings.

  // The initial position of the new marker is the intersection of the current
  // marker with the plot of the first data guide.

  assert(!visualisationEngine().isEmpty());

  double x, y;

  assert(markerEnabled(xMarker()) || markerEnabled(yMarker()));
  assert(!(markerEnabled(xMarker()) && markerEnabled(yMarker())));

  // Take the first guide.
  DataGuide const& guide = visualisationEngine().guide(0);

  if(markerEnabled(xMarker())) {
    // xMarker iterates over the y-axis. Attribute values will be shown in
    // the map.
    if(!intersectMarker(&x, &y, xMarker(), guide)) {
      y = 0.5;
    }

    // Snap to closest quantile.
    dal::DataSpace const& space = dataObject().dataSpace();
    assert(space.hasCumProbabilities());
    size_t index = space.indexOf(dal::CumulativeProbabilities);
    dal::Dimension const& dimension = space.dimension(index);
    dataObject().setQuantile(dimension.clamp<float>(y), false);

    dataObject().unsetSelectedValue(false);

    for(size_t i = 0; i < visualisationEngine().size(); ++i) {
      DataGuide const& guide = visualisationEngine().guide(i);
      dataObject().popClassifiers(guide, false);
    }
  }
  else if(markerEnabled(yMarker())) {
    // double min, max;

    // extremes(&min, &max);

    // yMarker iterates over the x-axis. Probabilities will be shown in the
    // map.
    if(!intersectMarker(&x, &y, yMarker(), guide)) {
      // Marker does not intersect the curve of the first guide.
#if QWT_VERSION >= 0x060100
      // QwtPlot::axisScaleDiv returns a reference.
      QwtScaleDiv const& scaleDiv = axisScaleDiv(xMarker());
#else
      // QwtPlot::axisScaleDiv returns a pointer.
      QwtScaleDiv const& scaleDiv = *axisScaleDiv(xMarker());
#endif

      x = scaleDiv.lowerBound();

      if(scaleDiv.range() > 0.0) {
        // Use the middle value.
        x += scaleDiv.range() / 2.0;
      }
    }

    dataObject().setSelectedValue(x, false);

    for(size_t i = 0; i < visualisationEngine().size(); ++i) {
      DataGuide const& guide = visualisationEngine().guide(i);
      assert(guide.valueScale() == VS_SCALAR);

      com::Classifier classifier(0.0, 1.0);
      classifier.setNrClasses(100);
      classifier.installAlgorithm(com::Classifier::LIN);
      dataObject().pushClassifier(guide, classifier, false);
    }
  }

  dataObject().notify();
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace ag
