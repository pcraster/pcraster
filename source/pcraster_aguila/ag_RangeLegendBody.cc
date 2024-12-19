#include "ag_RangeLegendBody.h"

// Std
#include <string>
#include <vector>

// Qt
#include <QApplication>
#include <QPainter>
#include <QFontMetrics>

// Pcr
#include "com_classifier.h"
#include "com_util.h"
#include "ag_DataObject.h"
#include "ag_DataProperties.h"
#include "ag_RangeDrawProps.h"



/*!
  \file
  This file contains the implementation of the RangeLegendBody class.
*/


namespace ag {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC CLASS MEMBERS
//------------------------------------------------------------------------------

size_t RangeLegendBody::_nrCreated(0);

QSize RangeLegendBody::_keyBoxOffset(0, 0);

int RangeLegendBody::_maxKeyBoxHeight(125);



//! Returns the offset of the keybox.
/*!
  \return    Offset of keybox.
*/
QSize const& RangeLegendBody::keyBoxOffset()
{
  return _keyBoxOffset;
}



//------------------------------------------------------------------------------
// DEFINITION OF CLASS MEMBERS
//------------------------------------------------------------------------------

//! Constructor.
/*!
*/
RangeLegendBody::RangeLegendBody(
         DataObject const& object,
         DataGuide const& guide,
         ViewerType type,
         QWidget* parent)

  : LegendBody(type, parent),
    _drawProperties(object.properties().rangeDrawProperties(guide))

{
  // Check if this is the first one we create. QApplication object has been
  // created so we can query it.
  if(++_nrCreated == 1) {
    // At the top of the legend body there is a tic with the maximum value.
    // The label of this value extents above the tic by at most half the height
    // of the font used to draw it.
    _keyBoxOffset.setHeight(static_cast<int>(
                   0.5 * QFontMetrics(qApp->font()).height()));
  }

  // Determine and set size of body.
  setFixedSize(width(), height());
}



//! Destructor.
/*!
*/
RangeLegendBody::~RangeLegendBody()
{
}



//! Return the label to use for class \a id.
/*!
  \param     id Class id, 0 is first class, with lowest values.
  \return    Label.

  A >= or <= sign is prepended when appropriate.
*/
QString RangeLegendBody::label(
         size_t id) const
{
  assert(id < _drawProperties.classBorders().size());

  QString result;

  if(id == 0) {
    if(_drawProperties.classBorders()[id] > _drawProperties.min()) {
      // 2264 <=
      result = QChar(0x2264);
    }
    // else {
    //   result = "<";
    // }
  }
  else if(id == _drawProperties.classBorders().size() - 1) {
    if(_drawProperties.classBorders()[id] < _drawProperties.max()) {
      // 2265 >=
      result = QChar(0x2265);
    }
    // else {
    //   result = ">";
    // }
  }

  // Cast to get at right label() function.
  result += QString(
         dynamic_cast<DrawProps const&>(_drawProperties).label(id).c_str());

  return result;
}



int RangeLegendBody::maxLengthLabel() const
{
  int length = 0;

  if(_drawProperties.rawValueClassifier()->algorithm() ==
       com::Classifier::USERDEFINED) {
#if QT_VERSION < QT_VERSION_CHECK(5, 11, 0)
    length = qApp->fontMetrics().width(QString("Not distinguishable"));
#else
    length = QFontMetrics(qApp->font()).horizontalAdvance(QString("Not distinguishable"));
#endif
  }
  else {
    for(size_t i = 0; i <= _drawProperties.nrClasses(); ++i) {
#if QT_VERSION < QT_VERSION_CHECK(5, 11, 0)
      length = std::max<int>(length, qApp->fontMetrics().width(label(i)));
#else
      length = std::max<int>(length, QFontMetrics(qApp->font()).horizontalAdvance(label(i)));
#endif
    }
  }

  return length;
}



int RangeLegendBody::keyBoxHeight() const
{
  return _drawProperties.drawerType() == VECTORS
#if QT_VERSION < QT_VERSION_CHECK(5, 11, 0)
         ? qApp->fontMetrics().width(QString("Cell length"))
#else
         ? QFontMetrics(qApp->font()).horizontalAdvance(QString("Cell length"))
#endif
         : _maxKeyBoxHeight;
}



void RangeLegendBody::paintKeyLegend()
{
  if(_drawProperties.nrClasses() == 0) {
    return;
  }

  std::vector<double> borders = _drawProperties.classBorders();
  int left, top;

  // maxCutoff is first border, minCutoff is last.
  std::reverse(borders.begin(), borders.end());

  assert(!borders.empty());

  // Values -> pixels.
  double y_factor = -1.0 * keyBoxHeight() / (_drawProperties.maxCutoff() - _drawProperties.minCutoff()) ;
  double y_offset = keyBoxHeight() - _drawProperties.minCutoff() * y_factor;

  QTransform map = QTransform(0, 0, 0, 0, y_factor, 0, 0, y_offset, 1);

  assert(qRound(map.map(QPointF(0, _drawProperties.maxCutoff())).y()) == 0);
  assert(qRound(map.map(QPointF(0, _drawProperties.minCutoff())).y()) == keyBoxHeight());

  left = keyBoxOffset().width();

  QPainter painter(this);

  if(_drawProperties.drawerType() == COLOURFILL) {
    // Legend body contains these parts (from top to bottom):
    // Keyboxes of the individual classes.
    // Rectangle around all of this.
    // Tic marks and labels.

    // painter.setPen(Qt::NoPen);

    // Draw keyboxes. --------------------------------------------------------
    for(size_t i = 0; i < (borders.size() - 1); ++i) {
      top = keyBoxOffset().height() + qRound(map.map(QPointF(0, borders[i])).y());
      painter.setPen(_drawProperties.colourByIndex(
            borders.size() - i - 2));
      painter.setBrush(_drawProperties.colourByIndex(
            borders.size() - i - 2));
      painter.drawRect(left, top, keySize().width(),
            qRound(map.map(QPointF(0, borders[i + 1])).y() -
                 map.map(QPointF(0, borders[i])).y() ));
    }

    // Draw the box *on top of* the keys. ------------------------------------
    top = keyBoxOffset().height();
    painter.setPen(palette().color(QPalette::WindowText));
    painter.setBrush(Qt::NoBrush);
    // Substract one row and column of pixels from the left and bottom of
    // the box, since the keys are drawn without a pen.
    painter.drawRect(left, top, keySize().width(), keyBoxHeight());
  }
  else if(_drawProperties.drawerType() == CONTOUR) {
    for(size_t i = 0; i < (borders.size() - 1); ++i) {
      top = keyBoxOffset().height()  + qRound(map.map(QPointF(0, borders[i])).y());
      painter.setPen(_drawProperties.colourByIndex(borders.size() - i - 2));
      painter.drawLine(left, top, left + keySize().width(), top);
    }
  }

  paintLabels(painter, map, borders);
}



void RangeLegendBody::paintVectorLegend()
{
  if(_drawProperties.nrClasses() == 0) {
    return;
  }

  std::vector<double> borders = _drawProperties.classBorders();
  assert(!borders.empty());

  // maxCutoff is first border, minCutoff is last.
  std::reverse(borders.begin(), borders.end());

  // Values -> pixels.
  double delta = -1.0 * keyBoxHeight() / (_drawProperties.maxCutoff() - _drawProperties.minCutoff()) ;
  double y_offset = keyBoxHeight() - _drawProperties.minCutoff() * delta;

  QTransform map = QTransform(0, 0, 0, 0, delta, 0, 0, y_offset, 1);

  assert(qRound(map.map(QPointF(0, _drawProperties.maxCutoff())).y()) == 0);
  assert(qRound(map.map(QPointF(0, _drawProperties.minCutoff())).y()) == keyBoxHeight());

  QPainter painter(this);
  painter.setRenderHint(QPainter::Antialiasing);

  {
    int center, top, bottom;
    center = keyBoxOffset().width() + keySize().width();
    bottom = keyBoxOffset().height() + qRound(map.map(QPointF(0, *(borders.end() - 1))).y());
    painter.setPen(Qt::black);

    // Draw keyboxes. --------------------------------------------------------
    for(size_t i = 0; i < (borders.size() - 1); ++i) {
      top = keyBoxOffset().height() + qRound(map.map(QPointF(0, borders[i])).y());

      painter.drawLine(center, bottom, center    , top    );
      painter.drawLine(center, top   , center - 3, top + 5);
      painter.drawLine(center, top   , center + 3, top + 5);
    }

    {
      int x = keyBoxOffset().width() + QFontMetrics(qApp->font()).height();
      int y = bottom;
      painter.translate(x, y);
      painter.rotate(-90);
      painter.drawText(0, 0, "Cell length");
      painter.rotate(90);
      painter.translate(-x, -y);
    }
  }

  paintLabels(painter, map, borders, false);
}



void RangeLegendBody::paintLabels(
         QPainter& painter,
         QTransform const& map,
         std::vector<double> const& borders,
         bool drawTics) const
{
  int left, top, right, bottom;

  // Determine which class borders to label, based on the amount of
  // available space. The lower and upper borders are always labeled,
  // even if there's no space available for them (which won't be common).

  // Determine a first, largest number of borders and classes.
  int nrBorders = borders.size();
  int nrClasses = nrBorders - 1;

  // Height available for drawing labels.
  int keyBoxHeightForLabels =
  qRound(map.map(QPointF(0, borders.back())).y()) -
  qRound(map.map(QPointF(0, borders.front())).y()) + 1;

  // Determine the number of borders which can be labeled, based on the
  // height of the current font.
  while(nrBorders > 2) {
    // Test if the labels fit.
    if(nrBorders * (QFontMetrics(qApp->font()).height()) <=
            (keyBoxHeightForLabels + QFontMetrics(qApp->font()).height())) {
      break;
    }
    else {
      nrClasses = com::largestDivisor(nrClasses, nrClasses - 1);
      nrBorders = nrClasses + 1;
    }
  }

  // Now we have the number of class borders to label.
  // Determine increment for the loop.
  assert(_drawProperties.nrClasses() % nrClasses == 0);
  size_t incr = _drawProperties.nrClasses() / nrClasses;

  // Draw tics and labels.
  left = keyBoxOffset().width() + keySize().width();
  right = left + ticLength();

  painter.setRenderHint(QPainter::Antialiasing, false);
  painter.setPen(palette().color(QPalette::WindowText));

  if(_drawProperties.rawValueClassifier()->algorithm() ==
       com::Classifier::USERDEFINED) {
    // Special legend. Draw labels for the case that confidence intervals
    // are shown.
    assert(borders.size() == 4);
    assert(incr == 1);

    assert(_drawProperties.probabilityScale() ==
            RangeDrawProps::CumulativeProbabilities ||
           _drawProperties.probabilityScale() ==
            RangeDrawProps::ExceedanceProbabilities);

    // Lower in case of cum prob, higher in case of exceed prob.
    double classHeightInPixels = map.map(QPointF(0, borders[1])).y() -
       map.map(QPointF(0, borders[0])).y();
    bottom = keyBoxOffset().height() + qRound(map.map(QPointF(0, borders[0])).y() +
       classHeightInPixels / 2.0);
    painter.drawText(right + labelOffset().width(),
            bottom + labelOffset().height(),
            _drawProperties.probabilityScale() ==
                 RangeDrawProps::CumulativeProbabilities
                      ? "Lower"
                      : "Higher");

    // Not distinguishable.
    classHeightInPixels = map.map(QPointF(0, borders[2])).y() -
       map.map(QPointF(0, borders[1])).y();
    bottom = keyBoxOffset().height() + qRound(map.map(QPointF(0, borders[1])).y() +
       classHeightInPixels / 2.0);
    painter.drawText(right + labelOffset().width(),
            bottom + labelOffset().height(), "Not distinguishable");

    // Higher in case of cum prob, lower in case of exceed prob.
    classHeightInPixels = map.map(QPointF(0, borders[3])).y() -
       map.map(QPointF(0, borders[2])).y();
    bottom = keyBoxOffset().height() + qRound(map.map(QPointF(0, borders[2])).y() +
       classHeightInPixels / 2.0);
    painter.drawText(right + labelOffset().width(),
            bottom + labelOffset().height(),
            _drawProperties.probabilityScale() ==
                 RangeDrawProps::CumulativeProbabilities
                      ? "Higher"
                      : "Lower");
  }
  else {
    // Regular legend.
    for(size_t i = 0; i < borders.size(); i += incr) {
      top = keyBoxOffset().height() + qRound(map.map(QPointF(0, borders[i])).y());
      bottom = top;

      if(drawTics) {
        painter.drawLine(left, top, right, bottom);
      }

      painter.drawText(right + labelOffset().width(),
              bottom + labelOffset().height(), label(borders.size() - i - 1));
    }
  }
}



void RangeLegendBody::paintLineLegend()
{
  QPainter painter(this);
  // painter.setPen(foregroundColor());
  painter.setPen(dynamic_cast<DrawProps const&>(_drawProperties).colour());

  int y = static_cast<int>(0.5 * QFontMetrics(qApp->font()).height());

  // Draw line.
  int left = 0;
  int right = left + keySize().width();
  painter.drawLine(left, y, right, y);

  // Draw label.
  // painter.drawText(right + labelOffset().width(), y + labelOffset().height(),
  //                  QString("flow direction"));

  painter.end();
}



void RangeLegendBody::paintEvent(
         QPaintEvent* /* event */)
{
  // Viewer type
  //   VT_MAP
  //   VT_GRAPH
  //
  // Value scale
  //   Scalar
  //   Directional
  //
  // Drawer type
  //   COLOURFILL
  //   CONTOUR
  //
  // VT_MAP   / SCALAR | DIRECTION / COLOURFILL           -> Key legend
  // VT_MAP   / SCALAR | DIRECTION / CONTOUR              -> Key legend
  // VT_GRAPH / SCALAR | DIRECTION / COLOURFILL | CONTOUR -> Line legend
  switch(viewerType()) {
    case VT_Map: {
      switch(_drawProperties.drawerType()) {
        case VECTORS: {
          paintVectorLegend();
          break;
        }
        default: {
          paintKeyLegend();
          break;
        }
      }

      break;
    }
    case VT_Graph: {
      paintLineLegend();
      break;
    }
    default: {
      assert(false);
      break;
    }
  }
}



//! Calculates and returns the width of the body.
/*!
  \return    Width of body.
*/
int RangeLegendBody::width() const
{
  int result = 0;

  switch(viewerType()) {
    case VT_Map: {
      if(!_drawProperties.rawClassBorders().empty()) {
        result += keySize().width();
        result += ticLength();
        result += labelOffset().width();
        result += maxLengthLabel();
      }

      break;
    }
    case VT_Graph: {
      result = keySize().width();
      // + labelOffset().width() + _data->widthLabel();

      break;
    }
    default: {
      assert(false);
      break;
    }
  }

  return result;
}



//! Calculates and returns the height of the body.
/*!
  \return    Height of body.
*/
int RangeLegendBody::height() const
{
  int result = 0;

  switch(viewerType()) {
    case VT_Map: {
      if(!_drawProperties.rawClassBorders().empty()) {
        result += QFontMetrics(qApp->font()).height();
        result += keyBoxHeight();
      }

      break;
    }
    case VT_Graph: {
      result = QFontMetrics(qApp->font()).height();

      break;
    }
    default: {
      assert(false);
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
