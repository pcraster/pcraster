#include "ag_PlotVisualisation.h"

// Library headers.
#include <boost/foreach.hpp>
#include <QApplication>
#include <QPainter>
#include <qwt_picker_machine.h>
#include <qwt_plot_curve.h>
#include <qwt_plot_grid.h>
#include <qwt_plot_marker.h>
#include <qwt_plot_picker.h>
#include <qwt_scale_engine.h>
#include "pcrtypes.h"

// PCRaster library headers.

// Module headers.
#include "ag_DataObject.h"
#include "ag_RangeDrawProps.h"
#include "ag_VisEngine.h"



/*!
  \file
  This file contains the implementation of the PlotVisualisation class.
*/



//------------------------------------------------------------------------------

/*
namespace ag {

class PlotVisualisationPrivate
{
public:

  PlotVisualisationPrivate()
  {
  }

  ~PlotVisualisationPrivate()
  {
  }

};

} // namespace ag
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC PLOTVISUALISATION MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF PLOTVISUALISATION MEMBERS
//------------------------------------------------------------------------------

namespace ag {

PlotVisualisation::PlotVisualisation(
         DataObject* object,
         std::string const& visualisationName,
         QWidget* parent,
         char const* /* name */)

  : QwtPlot(parent),
    IVisualisation(object, visualisationName),
    _xMarker(new QwtPlotMarker()),
    _yMarker(new QwtPlotMarker())

{
  setFocusPolicy(Qt::WheelFocus);

  // Attaching here, before calling setXValue/setYValue results in a dump.
  // Therefore attaching markers to the plot is handled in
  // setXMarker/setYMarker.
  // _xMarker->attach(this);
  // _yMarker->attach(this);

  QwtPlotGrid* grid = new QwtPlotGrid();
  grid->attach(this);
#if QWT_VERSION >= 0x060100
  grid->setMajorPen(QPen(Qt::lightGray, 0, Qt::DotLine));
  grid->setMinorPen(QPen(Qt::lightGray, 0, Qt::DotLine));
#else
  grid->setMajPen(QPen(Qt::lightGray, 0, Qt::DotLine));
  grid->setMinPen(QPen(Qt::lightGray, 0, Qt::DotLine));
#endif
  grid->enableX(true);
  grid->enableY(true);
  grid->enableXMin(true);
  grid->enableYMin(true);

  _xMarker->setLinePen(QPen(palette().color(QPalette::WindowText), 0,
         Qt::SolidLine));
  _yMarker->setLinePen(QPen(palette().color(QPalette::WindowText), 0,
         Qt::SolidLine));

  _xMarkerId = 1;
  _yMarkerId = 2;

  _picker = new QwtPlotPicker(canvas());
  connect(_picker, SIGNAL(selected(const QPointF&)),
          this, SLOT(selected(const QPointF&)));
  connect(_picker, SIGNAL(selected(const QRectF&)),
          this, SLOT(selected(const QRectF&)));
  connect(_picker, SIGNAL(selected(const QVector<QPointF>&)),
          this, SLOT(selected(const QVector<QPointF>&)));
  connect(_picker, SIGNAL(appended(const QPointF&)),
          this, SLOT(appended(const QPointF&)));
  connect(_picker, SIGNAL(moved(const QPointF&)),
          this, SLOT(moved(const QPointF&)));

  replot();
}



PlotVisualisation::~PlotVisualisation()
{
}



bool PlotVisualisation::close()
{
  return QwtPlot::close();
}



void PlotVisualisation::enableMarker(
         long int marker)
{
  assert(marker == _xMarkerId || marker == _yMarkerId);

  if(marker == _xMarkerId) {
    _xMarker->setLineStyle(QwtPlotMarker::VLine);
    _xMarkerEnabled = true;
  }
  else if(marker == _yMarkerId) {
    _yMarker->setLineStyle(QwtPlotMarker::HLine);
    _yMarkerEnabled = true;
  }
}



void PlotVisualisation::disableMarker(long int marker)
{
  assert(marker == _xMarkerId || marker == _yMarkerId);

  if(marker == _xMarkerId) {
    _xMarker->setLineStyle(QwtPlotMarker::NoLine);
    _xMarkerEnabled = false;
  }
  else if(marker == _yMarkerId) {
    _yMarker->setLineStyle(QwtPlotMarker::NoLine);
    _yMarkerEnabled = false;
  }
}



bool PlotVisualisation::markerEnabled(long int marker) const
{
  assert(marker == _xMarkerId || marker == _yMarkerId);

  bool result = false; // Shut up compiler.

  if(marker == _xMarkerId) {
    result = _xMarkerEnabled;
  }
  else if(marker == _yMarkerId) {
    result = _yMarkerEnabled;
  }

  return result;
}



long int PlotVisualisation::xMarker() const
{
  return _xMarkerId;
}



long int PlotVisualisation::yMarker() const
{
  return _yMarkerId;
}



void PlotVisualisation::setXMarker(double value)
{
  _xMarker->setXValue(value);
}



void PlotVisualisation::setYMarker(double value)
{
  _yMarker->setYValue(value);
}



void PlotVisualisation::attachMarkers()
{
  _xMarker->attach(this);
  _yMarker->attach(this);
}



void PlotVisualisation::detachMarkers()
{
  _xMarker->detach();
  _yMarker->detach();
}



void PlotVisualisation::drawCurve(
         DataGuide const& guide,
         double* x,
         double* y,
         size_t nrValues,
         QPen const& pen)
{
  // If missing values are present, a curve consists of several curve pieces.
  // Loop over values to find MV's.
  double* xEnd = x + nrValues;
  double* yEnd = y + nrValues;
  double* xSubBegin, *xSubEnd, *ySubBegin, *ySubEnd;
  size_t sizeOfSubRange;
  // long int curveId;

  xSubEnd = x;
  ySubEnd = y;

  while(xSubEnd != xEnd && ySubEnd != yEnd) {
    // Set begin at first non-MV.
    xSubBegin = xSubEnd;
    ySubBegin = ySubEnd;
    while(xSubBegin != xEnd && ySubBegin != yEnd &&
         (pcr::isMV(*xSubBegin) || pcr::isMV(*ySubBegin))) {
      ++xSubBegin;
      ++ySubBegin;
    }

    if(xSubBegin == xEnd || ySubBegin == yEnd) {
      break;
    }

    // Set end at first MV.
    sizeOfSubRange = 1;
    xSubEnd = xSubBegin + 1;
    ySubEnd = ySubBegin + 1;
    while(xSubEnd != xEnd && ySubEnd != yEnd &&
         !pcr::isMV(*xSubEnd) && !pcr::isMV(*ySubEnd)) {
      ++xSubEnd;
      ++ySubEnd;
      ++sizeOfSubRange;
    }

    // Insert piece of the total curve.
    QwtPlotCurve* item = new QwtPlotCurve();
    item->setRenderHint(QwtPlotItem::RenderAntialiased);
    item->attach(this);
    item->setPen(pen);
    item->setSamples(xSubBegin, ySubBegin, sizeOfSubRange);

    // curveId = insertCurve("");
    // assert(curveId);
    // setCurvePen(curveId, pen);
    // setCurveData(curveId, xSubBegin, ySubBegin, sizeOfSubRange);
    _curvesPerGuide[guide].push_back(item);
  }
}



//! Removes curves and markers from the plot.
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .

  No history about the plotted items remains after calling this function.
*/
void PlotVisualisation::clearPlot()
{
  detachItems(QwtPlotItem::Rtti_PlotCurve);
  _curvesPerGuide.clear();
  detachMarkers();

  // replot();
}



void PlotVisualisation::trackClickPoint()
{
  _picker->setStateMachine(new QwtPickerClickPointMachine);
}



void PlotVisualisation::trackDragPoint()
{
  _picker->setStateMachine(new QwtPickerDragPointMachine);
}



void PlotVisualisation::trackDragRect()
{
  _picker->setStateMachine(new QwtPickerDragRectMachine);
}



void PlotVisualisation::selected(
         QPointF const& /* point */)
{
}



void PlotVisualisation::selected(
         QRectF const& /* rect */)
{
}



void PlotVisualisation::selected(
         QVector<QPointF> const& /* array */)
{
}



void PlotVisualisation::appended(
         QPointF const& /* point */)
{
}



void PlotVisualisation::moved(
         QPointF const& /* point */)
{
}



bool PlotVisualisation::intersectMarker(
         double* x,
         double* y,
         long int marker,
         DataGuide const& guide) const
{
  QLineF markerLine;

  if(marker == _xMarkerId) {
    markerLine = QLineF(
#if QWT_VERSION >= 0x060100
         QPointF(_xMarker->xValue(), axisScaleDiv(yLeft).lowerBound()),
         QPointF(_xMarker->xValue(), axisScaleDiv(yLeft).upperBound())
#else
         QPointF(_xMarker->xValue(), axisScaleDiv(yLeft)->lowerBound()),
         QPointF(_xMarker->xValue(), axisScaleDiv(yLeft)->upperBound())
#endif
    );
  }
  else if(marker == _yMarkerId) {
    markerLine = QLineF(
#if QWT_VERSION >= 0x060100
         QPointF(axisScaleDiv(xBottom).lowerBound(), _yMarker->yValue()),
         QPointF(axisScaleDiv(xBottom).upperBound(), _yMarker->yValue())
#else
         QPointF(axisScaleDiv(xBottom)->lowerBound(), _yMarker->yValue()),
         QPointF(axisScaleDiv(xBottom)->upperBound(), _yMarker->yValue())
#endif
    );
  }

  QPointF point1, point2, intersection;
  bool intersectionFound = false;

  std::map<DataGuide, std::vector<QwtPlotCurve*> >::const_iterator it =
         _curvesPerGuide.find(guide);

  if(it != _curvesPerGuide.end()) {
    BOOST_FOREACH(QwtPlotCurve const* curve, (*it).second) {
      assert(curve);
      assert(curve->dataSize() > 1);

      point2 = curve->sample(0);

      for(int j = 1; j < static_cast<int>(curve->dataSize()); ++j) {
        point1 = point2;
        point2 = curve->sample(j);

        intersectionFound = (markerLine.intersect(QLineF(point1, point2),
              &intersection) == QLineF::BoundedIntersection);

        if(intersectionFound) {
          *x = intersection.x();
          *y = intersection.y();
          break;
        }
      }

      if(intersectionFound) {
        break;
      }
    }
  }

  return intersectionFound;
}



QPixmap PlotVisualisation::pixmap()
{

  QPixmap pixmap(QPixmap::grabWidget(this));

  return pixmap;
}



// #include <CGAL/Cartesian.h>
// #include <CGAL/intersections.h>
// bool PlotVisualisation::intersectMarker(
//          double* x,
//          double* y,
//          long int marker,
//          DataGuide const& guide) const
// {
//   typedef CGAL::Line_2<CGAL::Cartesian<double> > Line;
//   typedef CGAL::Segment_2<CGAL::Cartesian<double> > Segment;
//   typedef CGAL::Point_2<CGAL::Cartesian<double> > Point;
//   Line markerLine;
// 
//   if(marker == _xMarkerId) {
//     markerLine = Line(
//        Point(_xMarker->xValue(), 0.0),
//        Point(_xMarker->xValue(), 1.0));
//   }
//   else if(marker == _yMarkerId) {
//     markerLine = Line(
//        Point(0.0, _yMarker->yValue()),
//        Point(1.0, _yMarker->yValue()));
//   }
// 
//   Segment plotSegment;
//   Point point1, point2, intersection;
//   CGAL::Object result;
//   bool intersectionFound = false;
//   QwtPlotCurve const* curve;
// 
//   std::map<DataGuide, std::vector<QwtPlotCurve*> >::const_iterator it =
//          _curvesPerGuide.find(guide);
// 
//   if(it != _curvesPerGuide.end()) {
//     std::vector<QwtPlotCurve*> const& curves = (*it).second;
// 
//     for(size_t i = 0; i < curves.size(); ++i) {
//       curve = curves[i];
//       assert(curve);
//       assert(curve->dataSize() > 1);
// 
//       point2 = Point(curve->x(0), curve->y(0));
// 
//       for(int j = 1; j < curve->dataSize(); ++j) {
//         point1 = point2;
//         point2 = Point(curve->x(j), curve->y(j));
//         plotSegment = Segment(point1, point2);
//         result = CGAL::intersection(markerLine, plotSegment);
// 
//         if(CGAL::assign(intersection, result)) {
//           *x = intersection.x();
//           *y = intersection.y();
//           intersectionFound = true;
//           break;
//         }
//       }
// 
//       if(intersectionFound) {
//         break;
//       }
//     }
//   }
// 
//   return intersectionFound;
// }



bool PlotVisualisation::onlyCumulativeProbabilitiesShown() const
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



bool PlotVisualisation::onlyExceedanceProbabilitiesShown() const
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

} // namespace ag

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------


