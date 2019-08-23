#include "ag_LineMarker.h"

#include <QtGui/QPainter>

#include <iostream>





namespace ag {


LineMarker::LineMarker(QtCharts::QChart *chart):
    QGraphicsItem(chart),
    m_chart(chart)
{
  m_ymin = -1;
  m_ymax = -1;
  m_xmin = -1;
  m_xmax = -1;
  // Point p1 is 'attached' to the axis
  p1 = QPointF(m_xmin, m_ymin);
  p2 = QPointF(m_xmax, m_ymax);

}


QRectF LineMarker::boundingRect() const
{
  QRectF rect;
  rect.setLeft(qMin(m_rect.left(), m_chart->mapToPosition(p1).x()));
  rect.setRight(qMax(m_rect.right(), m_chart->mapToPosition(p2).x()));
  rect.setTop(qMin(m_rect.top(), m_chart->mapToPosition(p2).y()));
  rect.setBottom(qMax(m_rect.bottom(), m_chart->mapToPosition(p1).y()));
  return rect;
}


void LineMarker::paint(QPainter *painter, const QStyleOptionGraphicsItem *option, QWidget *widget)
{
  painter->setPen(QPen(Qt::gray, 2));

  QPainterPath linePath;

  linePath.moveTo(m_chart->mapToPosition(p1));
  linePath.lineTo(m_chart->mapToPosition(p2));

  painter->drawPath(linePath);

}


void LineMarker::set_x_interval(double xmin, double xmax){
  m_xmin = xmin;
  m_xmax = xmax;
}


void LineMarker::set_y_interval(double ymin, double ymax){
  m_ymin = ymin;
  m_ymax = ymax;
}


void LineMarker::setXValue(double value){
  p1 = QPointF(value, m_ymin);
  p2 = QPointF(value, m_ymax);
  m_xval = value;
  updateGeometry();
}


void LineMarker::setYValue(double value){
  p1 = QPointF(m_xmin, value);
  p2 = QPointF(m_xmax, value);
  m_yval = value;
  updateGeometry();
}


void LineMarker::updateGeometry()
{
  prepareGeometryChange();
}


double LineMarker::xMin(){
  return m_xmin;
}


double LineMarker::xMax(){
  return m_xmax;
}


double LineMarker::yMin(){
  return m_ymin;
}


double LineMarker::yMax(){
  return m_ymax;
}


double LineMarker::xValue(){
  return m_xval;
}


double LineMarker::yValue(){
  return m_yval;
}


} // namespace ag
