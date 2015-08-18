#include "ag_ClassLegendBody.h"
#include <cassert>
#include <QApplication>
#include <QPainter>
#include "ag_ClassDrawProps.h"
#include "ag_DataObject.h"
#include "ag_DataProperties.h"



/*!
  \file
  This file contains the implementation of the ClassLegendBody class.
*/



namespace ag {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC CLASS MEMBERS
//------------------------------------------------------------------------------

const int ClassLegendBody::d_keyOffset(5);

int ClassLegendBody::keyOffset()
{
  return d_keyOffset;
}



//------------------------------------------------------------------------------
// DEFINITION OF CLASS MEMBERS
//------------------------------------------------------------------------------

ClassLegendBody::ClassLegendBody(
         DataObject const& object,
         DataGuide const& guide,
         ViewerType type,
         QWidget* parent)

  : LegendBody(type, parent),
    d_guide(guide),
    d_drawProperties(object.properties().classDrawProperties(guide))

{
  // Determine and set size of body.
  setFixedSize(width(), height());
}



//! Destructor.
/*!
*/
ClassLegendBody::~ClassLegendBody()
{
}



int ClassLegendBody::maxWidthLabel() const
{
  int result = 0;
  std::string label;

  for(size_t i = 0; i < d_drawProperties.nrClasses(); ++i) {
    label = d_drawProperties.label(i);
    result = std::max<int>(result,
         qApp->fontMetrics().width(QString(label.c_str())));
         // qApp->desktop()->fontMetrics().width(QString(label.c_str())));
  }

  return result;
}



void ClassLegendBody::paintLineLegend()
{
  assert(d_guide.valueScale() == VS_LDD);

  QPainter painter(this);
  painter.setPen(palette().color(QPalette::WindowText));

  int y = static_cast<int>(0.5 * qApp->fontMetrics().height());

  // Draw line.
  int left = 0;
  int right = left + keySize().width();
  painter.drawLine(left, y, right, y);

  // Draw label.
  painter.drawText(right + labelOffset().width(), y + labelOffset().height(),
                   QString("flow direction"));

  painter.end();
}



void ClassLegendBody::paintKeyLegend()
{
  if(d_drawProperties.nrClasses() > 0) {
    int left, top;

    QPainter painter(this);

    // Draw the keys.
    left = 0;

    painter.setPen(palette().color(QPalette::WindowText));

    for(size_t i = 0; i < d_drawProperties.nrClasses(); ++i) {
      top = i * (keySize().height() + keyOffset());
      painter.setBrush(d_drawProperties.colourByIndex(i));
      painter.drawRect(left, top, keySize().width(), keySize().height());
    }

    // Draw labels.
    std::string label;
    left = keySize().width();
    for(size_t i = 0; i < d_drawProperties.nrClasses(); ++i) {
      top = (i + 1) * (keySize().height() + keyOffset());
      label = d_drawProperties.label(i);
      painter.drawText(left + labelOffset().width(), top - keyOffset() -
                   static_cast<int>(0.5 * keySize().height()) +
                   labelOffset().height(), QString(label.c_str()));
    }

    painter.end();
  }
}



void ClassLegendBody::paintEvent(QPaintEvent * /* event */)
{
  // Viewer type
  //   VT_MAP
  //
  // Value scale
  //   LDD
  //   BOOLEAN
  //   NOMINAL
  //   ORDINAL
  //
  // VT_MAP   / LDD                         -> Line legend
  // VT_MAP   / BOOLEAN | NOMINAL | ORDINAL -> Key legend
  switch(d_guide.valueScale()) {
    case VS_LDD: {
      paintLineLegend();
      break;
    }
    case VS_BOOLEAN:
    case VS_NOMINAL:
    case VS_ORDINAL: {
      paintKeyLegend();
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
  \sa        height()
*/
int ClassLegendBody::width() const
{
  int result = 0;

  switch(d_guide.valueScale()) {
    case VS_LDD: {
      result = keySize().width() + labelOffset().width() +
         qApp->fontMetrics().width("flow direction");

      break;
    }
    case VS_BOOLEAN:
    case VS_NOMINAL:
    case VS_ORDINAL: {
      if(d_drawProperties.nrClasses()) {
        result += keySize().width();
        result += labelOffset().width();
        result += maxWidthLabel();
      }

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
  \sa        width()
*/
int ClassLegendBody::height() const
{
  int result = 0;

  switch(d_guide.valueScale()) {
    case VS_LDD: {
      result = qApp->fontMetrics().height();

      break;
    }
    case VS_BOOLEAN:
    case VS_NOMINAL:
    case VS_ORDINAL: {
      if(d_drawProperties.nrClasses()) {
        result =
          // n keys + space inbetween
          (d_drawProperties.nrClasses() - 1) * (keySize().height() +
            keyOffset()) + keySize().height() +
          // We need an additional row of pixels.
          1 +
          // 0.5 fontsize because some letters extent below the base line
          // (g, y, etc).
          0.5 * qApp->fontMetrics().height()
          ;
      }

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
