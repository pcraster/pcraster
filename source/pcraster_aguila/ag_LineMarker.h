#pragma once

#include <QGraphicsSceneHoverEvent>
#include <QtWidgets/QGraphicsItem>
#include <QtCharts/QChart>





namespace ag {

class LineMarker : public QGraphicsItem
{
public:
  LineMarker(QtCharts::QChart *parent);

  QRectF boundingRect() const;
  void paint(QPainter *painter, const QStyleOptionGraphicsItem *option,QWidget *widget);

  void setAnchor(QPointF point);
  void updateGeometry();

  void setXValue(double value);

  void set_y_interval(double ymin, double ymax);

protected:


private:
     QRectF m_rect;
     QPointF m_anchor;
     QtCharts::QChart *m_chart;

     double m_ymin, m_ymax;
};


} // namespace ag
