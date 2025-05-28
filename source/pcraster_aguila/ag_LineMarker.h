#ifndef INCLUDED_AG_LINEMARKER
#define INCLUDED_AG_LINEMARKER

#include <QGraphicsSceneHoverEvent>
#include <QtWidgets/QGraphicsItem>
#include <QtCharts/QChart>





namespace ag {

class LineMarker : public QGraphicsItem
{
public:
#if QT_VERSION < QT_VERSION_CHECK(6, 0, 0)
  LineMarker(QtCharts::QChart *parent);
#else
  LineMarker(QChart *parent);
#endif

  QRectF boundingRect() const override;

  void paint(QPainter *painter, const QStyleOptionGraphicsItem *option,QWidget *widget) override;

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

#if QT_VERSION < QT_VERSION_CHECK(6, 0, 0)
  QtCharts::QChart *m_chart;
#else
  QChart *m_chart;
#endif


  QRectF m_rect;

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

#endif