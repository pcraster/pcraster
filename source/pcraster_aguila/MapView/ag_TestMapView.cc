#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_AG_TESTMAPVIEW
#include "ag_TestMapView.h"
#define INCLUDED_AG_TESTMAPVIEW
#endif

// Library headers.
#ifndef INCLUDED_QAPPLICATION
#include <QApplication>
#define INCLUDED_QAPPLICATION
#endif

#ifndef INCLUDED_QPAINTER
#include <QPainter>
#define INCLUDED_QPAINTER
#endif

#ifndef INCLUDED_QPAINTEVENT
#include <QPaintEvent>
#define INCLUDED_QPAINTEVENT
#endif

// PCRaster library headers.

// Module headers.



/*!
  \file
  This file contains the implementation of the TestMapView class.
*/



int main(
  int argc,
  char** argv)
{
  QApplication application(argc, argv);

  // ag::TestMapView view1(ag::BufferedWidget::Center);
  ag::TestMapView view2(ag::BufferedWidget::TopLeft);

  // view1.show();
  view2.show();

  return application.exec();
}



namespace ag {

// Code that is private to this module.
namespace detail {

} // namespace detail



//------------------------------------------------------------------------------
// DEFINITION OF STATIC TESTMAPVIEW MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF TESTMAPVIEW MEMBERS
//------------------------------------------------------------------------------

TestMapView::TestMapView(
         Alignment alignment,
         QWidget* parent)

  : MapView(alignment, parent)

{
  // Assume a map:
  // upper left : -500.0,  1000.0
  // lower right:  500.0, -1000.0
  // initializeExtent(QRectF(-500.0, 1000.0, 1000.0, 2000.0));
  // initializeExtent(QRectF(1000.0, -500.0, 2000.0, 1000.0));

  initializeExtent(QRectF(10.0, 10.0, 20.0, 30.0));
}



TestMapView::~TestMapView()
{
}



void TestMapView::updateBuffer(
         QRect const& area)
{
  QPainter painter(&buffer());
  // painter.setClipRect(area);      // Only paint in the dirty area.

  // Fill background of dirty area. Clean slate to draw map on.
  painter.fillRect(area, palette().color(QPalette::Window));

  // Configure transformation matrix.
  painter.setWorldTransform(worldTransform());

  // painter.scale(scale(), scale());
  // painter.translate(translation());

  // Determine part of map to update.
  // Add one pixel on all sides of the dirty area to account for rounding
  // errors.
  QRect dirtyArea(area.left() - 1, area.top() - 1,
         area.width() + 2, area.height() + 2);
  QRectF mapArea(painter.worldTransform().inverted().mapRect(
         QRectF(dirtyArea)));

  // TODO verdeel verantwoordelijkheid buffered widget en mapview
  // bufferedw gaat over buffer management en efficiente update opdrachten
  // genereren.
  // mapview gaat over een kaart, met zoom en pan settings. dergelijke settings
  // kunnen uit de buff w lijkt me
  mapArea &= extent().translated(
         -extent().left() - (extent().width() / 2.0),
         -extent().top() - (extent().height() / 2.0));

  // Draw code for the map below this line. ------------------------------------
  static int red = 0;
  static int green = 100;
  static int blue = 200;

  red = (red + 30) % 255;
  green = (green + 30) % 255;
  blue = (blue + 30) % 255;

  // Draw part of the map that is dirty.
  painter.fillRect(mapArea, QBrush(QColor(red, green, blue)));
}



void TestMapView::mousePressEvent(
         QMouseEvent* event)
{
}



void TestMapView::mouseMoveEvent(
         QMouseEvent* event)
{
}



void TestMapView::mouseReleaseEvent(
         QMouseEvent* event)
{
}



void TestMapView::mouseDoubleClickEvent(
         QMouseEvent* event)
{
  if(event->button() == Qt::LeftButton) {
    focusOn(event->pos());
    zoomBy(1.2);
    // zoomTo(event->pos(), 1.2);
  }
  else if(event->button() == Qt::RightButton) {
    focusOn(event->pos());
    zoomBy(0.8);
    // zoomTo(event->pos(), 0.8);
  }

  update();
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace ag

