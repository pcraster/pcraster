#pragma once

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
protected:
//     void mousePressEvent(QGraphicsSceneMouseEvent *event);
//     void mouseMoveEvent(QGraphicsSceneMouseEvent *event);

private:
     QRectF m_rect;
     QPointF m_anchor;
     QtCharts::QChart *m_chart;
};


} // namespace ag
