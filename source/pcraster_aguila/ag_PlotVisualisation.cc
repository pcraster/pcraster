#include "ag_PlotVisualisation.h"

// Library headers.
#include <QApplication>
#include <QPainter>
#include <QtCharts/QLineSeries>
#include <QtGui/QMouseEvent>
#include <QtGlobal>


// PCRaster library headers.
#include "pcrtypes.h"

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

#if QT_VERSION < QT_VERSION_CHECK(6, 0, 0)
using namespace QtCharts;
#endif

PlotVisualisation::PlotVisualisation(
         DataObject* object,
         std::string const& visualisationName,
         QWidget* parent,
         char const* /* name */)

  :
    QChartView(parent),
    IVisualisation(object, visualisationName)

{
  setFocusPolicy(Qt::WheelFocus);

  setDragMode(QGraphicsView::NoDrag);
  setVerticalScrollBarPolicy(Qt::ScrollBarAlwaysOff);
  setHorizontalScrollBarPolicy(Qt::ScrollBarAlwaysOff);


  m_chart = new QChart;
  m_chart->legend()->hide();

  m_axisX = new QValueAxis;
  m_axisY = new QValueAxis;

  setRenderHint(QPainter::Antialiasing);
  this->setChart(m_chart);
  m_chart->addAxis(m_axisX, Qt::AlignBottom);
  m_chart->addAxis(m_axisY, Qt::AlignLeft);

  // We use one line marker to mimic the old behaviour of two separate markers
  _xMarker = new LineMarker(m_chart);

  _xMarkerId = 1;
  _yMarkerId = 2;

// // // // //
// // // // //   _picker = new QwtPlotPicker(canvas());
// // // // //   connect(_picker, SIGNAL(selected(const QPointF&)),
// // // // //           this, SLOT(selected(const QPointF&)));
// // // // //   connect(_picker, SIGNAL(selected(const QRectF&)),
// // // // //           this, SLOT(selected(const QRectF&)));
// // // // //   connect(_picker, SIGNAL(selected(const QVector<QPointF>&)),
// // // // //           this, SLOT(selected(const QVector<QPointF>&)));
// // // // //   connect(_picker, SIGNAL(appended(const QPointF&)),
// // // // //           this, SLOT(appended(const QPointF&)));
// // // // //   connect(_picker, SIGNAL(moved(const QPointF&)),
// // // // //           this, SLOT(moved(const QPointF&)));

  update();
}



PlotVisualisation::~PlotVisualisation()
{
}



bool PlotVisualisation::close()
{
  // return QwtPlot::close();
  return true;
}



void PlotVisualisation::enableMarker(
         long int marker)
{
  assert(marker == _xMarkerId || marker == _yMarkerId);

  if(marker == _xMarkerId) {
    _xMarker->set_y_interval(m_axisY->min(), m_axisY->max());
    _xMarker->set_x_interval(m_axisX->min(), m_axisX->max());
    _xMarkerEnabled = true;
  }
  else if(marker == _yMarkerId) {
    _xMarker->set_x_interval(m_axisX->min(), m_axisX->max());
    _xMarker->set_y_interval(0, 1);
    _yMarkerEnabled = true;

  }
}



void PlotVisualisation::disableMarker(long int marker)
{
  assert(marker == _xMarkerId || marker == _yMarkerId);

  if(marker == _xMarkerId) {
    _xMarkerEnabled = false;
  }
  else if(marker == _yMarkerId) {
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
  _xMarker->setYValue(value);
}



void PlotVisualisation::attachMarkers()
{
  _xMarker->set_y_interval(m_axisY->min(), m_axisY->max());
  _xMarker->set_x_interval(m_axisX->min(), m_axisX->max());
}



void PlotVisualisation::detachMarkers()
{
// // // // // // // // // // //   _xMarker->detach();
// // // // // // // // // // //   _yMarker->detach();
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

    auto *series = new QLineSeries();

    // Plain inserting the values...
    for(size_t i = 0; i < nrValues; ++i){
      series->append(x[i], y[i]);
    }

    series->setPen(pen);
    _curvesPerGuide[guide].push_back(series);
    m_chart->addSeries(series);
    series->attachAxis(m_axisY);
    series->attachAxis(m_axisX);
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
  m_chart->removeAxis(m_axisY);
  m_chart->removeAxis(m_axisX);
  m_chart->removeAllSeries();
  _curvesPerGuide.clear();
}



void PlotVisualisation::trackClickPoint()
{
// // // // // //   _picker->setStateMachine(new QwtPickerClickPointMachine);
}



void PlotVisualisation::trackDragPoint()
{
// // // // // //   _picker->setStateMachine(new QwtPickerDragPointMachine);
}



void PlotVisualisation::trackDragRect()
{
// // // // // //   _picker->setStateMachine(new QwtPickerDragRectMachine);
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
         QPointF(_xMarker->xValue(), _xMarker->xMin()),
         QPointF(_xMarker->xValue(), _xMarker->xMax())
    );
  }
  else if(marker == _yMarkerId) {
    markerLine = QLineF(
         QPointF(_xMarker->xMin(), _xMarker->yValue()),
         QPointF(_xMarker->xMax(), _xMarker->yValue())
    );
  }

  QPointF point1, point2, intersection;
  bool intersectionFound = false;

  auto it =
         _curvesPerGuide.find(guide);

  if(it != _curvesPerGuide.end()) {
    for(QLineSeries const* curve : (*it).second) {
      assert(curve);
      assert(curve->pointsVector().length() > 1);

      point2 = curve->at(0);

      for(int j = 1; j < static_cast<int>(curve->pointsVector().length()); ++j) {
        point1 = point2;
        point2 = curve->at(j);
#if QT_VERSION < QT_VERSION_CHECK(5, 14, 0)
        intersectionFound = (markerLine.intersect(QLineF(point1, point2),
#else
        intersectionFound = (markerLine.intersects(QLineF(point1, point2),
#endif
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
#if QT_VERSION < QT_VERSION_CHECK(5, 13, 0)
  QPixmap pixmap(QPixmap::grabWidget(this));
#else
  QPixmap pixmap(QWidget::grab());
#endif

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

// Moves the marker to the corresponding axis position
void PlotVisualisation::mousePressEvent(QMouseEvent *event)
{
  double xval = m_chart->mapToValue(event->pos()).x();
  double yval = m_chart->mapToValue(event->pos()).y();

  if(markerEnabled(1) == true) {
    xval = std::max(xval, m_axisX->min());
    xval = std::min(xval, m_axisX->max());

    moved(QPointF(xval, 0));
  }
  else {
    yval = std::max(yval, 0.0);
    yval = std::min(yval, 1.0);

    moved(QPointF(xval,yval));
  }

  update();
}


// Drags the marker to the corresponding axis position
void PlotVisualisation::mouseMoveEvent(QMouseEvent *event)
{
  if (event->buttons() & Qt::LeftButton){
    mousePressEvent(event);
  }
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


