#include "ag_RangeLegendBody.h"

// Std
#include <string>
#include <vector>

// Qt
#include <QApplication>
#include <QPainter>
#include <qwt_scale_map.h>

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

int RangeLegendBody::_maxKeyBoxHeight(120);



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
                   0.5 * qApp->fontMetrics().height()));
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
    length = qApp->fontMetrics().width(QString("Not distinguishable"));
  }
  else {
    for(size_t i = 0; i <= _drawProperties.nrClasses(); ++i) {
      length = std::max<int>(length, qApp->fontMetrics().width(label(i)));
    }
  }

  return length;
}



int RangeLegendBody::keyBoxHeight() const
{

  return _drawProperties.drawerType() == VECTORS
         ? qApp->fontMetrics().width(QString("Cell length"))
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
  QwtScaleMap map;
  map.setScaleInterval(_drawProperties.maxCutoff(),
       _drawProperties.minCutoff());
  map.setPaintInterval(0, keyBoxHeight());

  assert(qRound(map.transform(_drawProperties.maxCutoff())) == 0);
  assert(qRound(map.transform(
       _drawProperties.minCutoff())) == keyBoxHeight());

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
      top = keyBoxOffset().height() + qRound(map.transform(borders[i]));
      painter.setPen(_drawProperties.colourByIndex(
            borders.size() - i - 2));
      painter.setBrush(_drawProperties.colourByIndex(
            borders.size() - i - 2));
      painter.drawRect(left, top, keySize().width(),
            qRound(map.transform(borders[i + 1]) -
                 map.transform(borders[i])));
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
      top = keyBoxOffset().height() + qRound(map.transform(borders[i]));
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
  QwtScaleMap map;
  map.setScaleInterval(_drawProperties.maxCutoff(),
       _drawProperties.minCutoff());
  map.setPaintInterval(0, keyBoxHeight());

  assert(qRound(map.transform(_drawProperties.maxCutoff())) == 0);
  assert(qRound(map.transform(
       _drawProperties.minCutoff())) == keyBoxHeight());

  QPainter painter(this);
  painter.setRenderHint(QPainter::Antialiasing);

  {
    int center, top, bottom;
    center = keyBoxOffset().width() + keySize().width();
    bottom = keyBoxOffset().height() +
         qRound(map.transform(*(borders.end() - 1)));

    painter.setPen(Qt::black);

    // Draw keyboxes. --------------------------------------------------------
    for(size_t i = 0; i < (borders.size() - 1); ++i) {
      top = keyBoxOffset().height() + qRound(map.transform(borders[i]));

      painter.drawLine(center, bottom, center    , top    );
      painter.drawLine(center, top   , center - 3, top + 5);
      painter.drawLine(center, top   , center + 3, top + 5);
    }

    {
      int x = keyBoxOffset().width() + qApp->fontMetrics().height();
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
         QwtScaleMap const& map,
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
       qRound(map.transform(borders.back())) -
       qRound(map.transform(borders.front())) + 1;

  // Determine the number of borders which can be labeled, based on the
  // height of the current font.
  while(nrBorders > 2) {
    // Test if the labels fit.
    if(nrBorders * (qApp->fontMetrics().height()) <=
            (keyBoxHeightForLabels + qApp->fontMetrics().height())) {
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
    double classHeightInPixels = map.transform(borders[1]) -
       map.transform(borders[0]);
    bottom = keyBoxOffset().height() + qRound(map.transform(borders[0]) +
       classHeightInPixels / 2.0);
    painter.drawText(right + labelOffset().width(),
            bottom + labelOffset().height(),
            _drawProperties.probabilityScale() ==
                 RangeDrawProps::CumulativeProbabilities
                      ? "Lower"
                      : "Higher");

    // Not distinguishable.
    classHeightInPixels = map.transform(borders[2]) -
       map.transform(borders[1]);
    bottom = keyBoxOffset().height() + qRound(map.transform(borders[1]) +
       classHeightInPixels / 2.0);
    painter.drawText(right + labelOffset().width(),
            bottom + labelOffset().height(), "Not distinguishable");

    // Higher in case of cum prob, lower in case of exceed prob.
    classHeightInPixels = map.transform(borders[3]) -
       map.transform(borders[2]);
    bottom = keyBoxOffset().height() + qRound(map.transform(borders[2]) +
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
      top = keyBoxOffset().height() + qRound(map.transform(borders[i]));
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

  int y = static_cast<int>(0.5 * qApp->fontMetrics().height());

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
        result += qApp->fontMetrics().height();
        result += keyBoxHeight();
      }

      break;
    }
    case VT_Graph: {
      result = qApp->fontMetrics().height();

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
