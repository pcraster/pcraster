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

  void updateGeometry();

  void setXValue(double value);

  void setYValue(double value);

  void set_x_interval(double ymin, double ymax);

  void set_y_interval(double ymin, double ymax);

  double xValue();

  double yValue();

  double xMin();

  double xMax();

  double yMin();

  double yMax();


protected:

private:

  QtCharts::QChart *m_chart;

  QRectF m_rect;

  QPointF m_anchor;

  double m_xmin;

  double m_xmax;

  double m_ymin;

  double m_ymax;

  QPointF p1;

  QPointF p2;

  double m_xval;

  double m_yval;

};


} // namespace ag
