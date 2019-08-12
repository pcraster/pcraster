#include "ag_LineMarker.h"

#include <QtGui/QPainter>

#include <iostream>





namespace ag {


LineMarker::LineMarker(QtCharts::QChart *chart):
    QGraphicsItem(chart),
    m_chart(chart)
{
}


QRectF LineMarker::boundingRect() const
{
    QPointF anchor = mapFromParent(m_chart->mapToPosition(m_anchor));
    QRectF rect;
    rect.setLeft(qMin(m_rect.left(), anchor.x()));
    rect.setRight(qMax(m_rect.right(), anchor.x()));
    rect.setTop(qMin(m_rect.top(), anchor.y()));
    rect.setBottom(qMax(m_rect.bottom(), anchor.y()));
    return rect;
}


void LineMarker::paint(QPainter *painter, const QStyleOptionGraphicsItem *option, QWidget *widget)
{
  QPainterPath linePath;
  painter->setPen(QPen(Qt::gray, 2));

  linePath.moveTo(m_chart->mapToPosition(m_anchor).x(), m_chart->mapToPosition(QPointF(0, m_ymin)).y());
  linePath.lineTo(m_chart->mapToPosition(m_anchor).x(), m_chart->mapToPosition(QPointF(0, m_ymax)).y());

  painter->drawPath(linePath);
}


void LineMarker::setAnchor(QPointF point)
{
  m_anchor = point;
}

void LineMarker::set_y_interval(double ymin, double ymax){
  m_ymin = ymin;
  m_ymax = ymax;
}


void LineMarker::setXValue(double value){
  setAnchor(QPointF(value, 0));
  updateGeometry();
}

void LineMarker::updateGeometry()
{
  prepareGeometryChange();
}







} // namespace ag
