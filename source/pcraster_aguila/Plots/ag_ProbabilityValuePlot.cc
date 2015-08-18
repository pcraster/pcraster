#ifndef INCLUDED_AG_PROBABILITYVALUEPLOT
#include "ag_ProbabilityValuePlot.h"
#define INCLUDED_AG_PROBABILITYVALUEPLOT
#endif

// External headers.
#ifndef INCLUDED_BOOST_SCOPED_ARRAY
#include <boost/scoped_array.hpp>
#define INCLUDED_BOOST_SCOPED_ARRAY
#endif

#ifndef INCLUDED_QAPPLICATION
#include <QApplication>
#define INCLUDED_QAPPLICATION
#endif

#ifndef INCLUDED_QPEN
#include <QPen>
#define INCLUDED_QPEN
#endif

// Project headers.

// Module headers.
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
  This file contains the implementation of the ProbabilityValuePlot class.
*/

namespace {

} // Anonymous namespace



namespace ag {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC PROBABILITYVALUEPLOT MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF PROBABILITYVALUEPLOT MEMBERS
//------------------------------------------------------------------------------

ProbabilityValuePlot::ProbabilityValuePlot(
         DataObject* object,
         QWidget* parent)

  : PlotVisualisation(object, "Probability Value Plot", parent),
    ProbabilityPlot()

{
  // Supported data types.
  std::vector<geo::DataType> dataTypes;
  dataTypes.push_back(geo::STACK);
  dataTypes.push_back(geo::FEATURE);
  setSupportedDataTypes(dataTypes);

  std::vector<CSF_VS> valueScales;
  valueScales.push_back(VS_SCALAR);
  setSupportedValueScales(valueScales);

  QwtText title;
  title.setFont(QApplication::font());

  title.setText(QString("value"));
  setAxisTitle(xBottom, title);

  setAxisScale(yLeft, 0.0, 1.0, 0.1);
  title.setText(QString("probability"));
  setAxisTitle(yLeft, title);

  trackDragPoint();
}



ProbabilityValuePlot::~ProbabilityValuePlot()
{
}



void ProbabilityValuePlot::rescan()
{
  visualisationEngine().rescan(dataObject());
}



void ProbabilityValuePlot::process()
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



void ProbabilityValuePlot::visualise()
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



void ProbabilityValuePlot::addAttribute(
         DataGuide const& dataGuide)
{
  testDataGuide(dataGuide);
  visualisationEngine().addAttribute(dataObject(), dataGuide);
}



void ProbabilityValuePlot::toggleMarker()
{
  // Determine which marker is on: the one which iterates over the quantiles
  // or the one which iterates over the data values.
  // Then switch from the one marker to the other and adjust relevant settings.

  // The initial position of the new marker is the intersection of the current
  // marker with the plot of the first data guide.

  assert(!visualisationEngine().isEmpty());

  REAL4 min, max;

  extremes(&min, &max);

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
    // yMarker iterates over the x-axis. Probabilities will be show in the
    // map.
    if(!intersectMarker(&x, &y, yMarker(), guide)) {
      assert(min < max);
      x = dal::comparable<double>(min, max) ? min : min + (max - min) / 2.0;
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



void ProbabilityValuePlot::appended(
         QwtDoublePoint const& point)
{
  moved(point);
}



void ProbabilityValuePlot::moved(
         QwtDoublePoint const& point)
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



QSize ProbabilityValuePlot::minimumSizeHint() const
{
  // Override QwtPlot one with the default.
  return QWidget::minimumSizeHint();
}



void ProbabilityValuePlot::createPlot()
{
  DataObject& dataObject(this->dataObject());
  dal::DataSpace const& space(dataObject.dataSpace());
  dal::DataSpaceAddress const& address(dataObject.dataSpaceAddress());

  if(!visualisationEngine().isEmpty() && space.hasCumProbabilities()) {
    size_t indexOfCumProbabilities = space.indexOf(
       dal::CumulativeProbabilities);
    if(address.isValid(indexOfCumProbabilities)) {
      REAL4 min, max;

      extremes(&min, &max);

      if(!pcr::isMV(min) && !pcr::isMV(max)) {

        SpatialDataset* dataset;

        for(size_t i = 0; i < visualisationEngine().size(); ++i) {
          DataGuide const& guide(visualisationEngine().guide(i));
          assert(guide.type() == geo::STACK || guide.type() == geo::FEATURE);
          assert(guide.valueScale() == VS_SCALAR);

          dataset = 0;

          switch(guide.type()) {
            case geo::STACK: {
              dataset = &dataObject.rasterDataSources().data(guide);
              break;
            }
            case geo::FEATURE: {
              dataset = &dataObject.featureDataSources().data(guide);
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
              dataObject.properties().rangeDrawProperties(guide));

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

            if(dataObject.isSelected(guide)) {
              pen = QPen(dataObject.properties().colour(guide), 2,
                   Qt::SolidLine);
            }
            else {
              pen = QPen(dataObject.properties().colour(guide), 1,
                   Qt::SolidLine);
            }

            drawCurve(guide, x.get(), y.get(), table.nrRecs(), pen);
          }
        }
      }

      setAxisScale(xBottom, min, max);

      // Configure title y-axis, based on the type of graph.
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

      if(dataObject.hasSelectedValue(/* guide */)) {
        // A value is selected. Use the first guide for the properties of
        // the marker. Mark data values.
        setXMarker(dataObject.selectedValue(/* guide */));
        enableMarker(xMarker());
        disableMarker(yMarker());
      }
      else {
        float quantile = address.coordinate<float>(indexOfCumProbabilities);

        setYMarker(quantile);
        disableMarker(xMarker());
        enableMarker(yMarker());
      }
    }
  }

  attachMarkers();
}



void ProbabilityValuePlot::extremes(
         REAL4* const min,
         REAL4* const max)
{
  pcr::setMV(*min);
  pcr::setMV(*max);

  SpatialDataset* dataset;

  for(size_t i = 0; i < visualisationEngine().size(); ++i) {
    DataGuide const& guide(visualisationEngine().guide(i));
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

///     RangeDrawProps const& properties(
///        dataObject().properties().rangeDrawProperties(guide));
/// 
    /// if(properties.cutoffsAreValid()) {
    ///   if(pcr::isMV(*min)) {
    ///     *min = properties.minCutoff();
    ///     *max = properties.maxCutoff();
    ///   }
    ///   else {
    ///     *min = std::min(*min, properties.minCutoff());
    ///     *max = std::max(*max, properties.maxCutoff());
    ///   }
    /// }
    if(!dataset->allMV()) {
      if(pcr::isMV(min)) {
        *min = dataset->min<REAL4>();
      }
      else {
        *min = std::min(*min, dataset->min<REAL4>());
      }

      if(pcr::isMV(max)) {
        *max = dataset->max<REAL4>();
      }
      else {
        *max = std::max(*max, dataset->max<REAL4>());
      }
    }
  }
}



bool ProbabilityValuePlot::onlyCumulativeProbabilitiesShown() const
{
  bool result = true;

  for(size_t i = 0; i < visualisationEngine().size(); ++i) {
    DataGuide const& guide(visualisationEngine().guide(i));
    RangeDrawProps const& properties(
         dataObject().properties().rangeDrawProperties(guide));

    if(properties.probabilityScale() !=
         RangeDrawProps::CumulativeProbabilities) {
      result = false;
      break;
    }
  }

  return result;
}



bool ProbabilityValuePlot::onlyExceedanceProbabilitiesShown() const
{
  bool result = true;

  for(size_t i = 0; i < visualisationEngine().size(); ++i) {
    DataGuide const& guide(visualisationEngine().guide(i));
    RangeDrawProps const& properties(
         dataObject().properties().rangeDrawProperties(guide));

    if(properties.probabilityScale() !=
         RangeDrawProps::ExceedanceProbabilities) {
      result = false;
      break;
    }
  }

  return result;
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace ag

