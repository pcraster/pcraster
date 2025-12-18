#ifndef INCLUDED_AG_PLOTVISUALISATION
#define INCLUDED_AG_PLOTVISUALISATION

#include "ag_DataGuide.h"
#include "ag_IVisualisation.h"
#include "ag_LineMarker.h"

#include <QtCharts/QChartView>
#include <QtCharts/QLineSeries>
#include <QtCharts/QValueAxis>
#include <QtWidgets/QGraphicsView>

#include <map>


namespace ag {
  // PlotVisualisation declarations.
  class DataObject;
}


namespace ag {



//! short_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
/*!
  longer_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
*/
#if QT_VERSION < QT_VERSION_CHECK(6, 0, 0)
class PlotVisualisation: public QtCharts::QChartView,
                         public ag::IVisualisation
#else
class PlotVisualisation: public QChartView,
                         public ag::IVisualisation
#endif
{

private:

  Q_OBJECT

  LineMarker*       _xMarker;

  //LineMarker*       _yMarker;

  long int         _xMarkerId;

  bool             _xMarkerEnabled{};

  long int         _yMarkerId;

  bool             _yMarkerEnabled{};

  long int         _selectedMarkerId{};

#if QT_VERSION < QT_VERSION_CHECK(6, 0, 0)
  std::map<DataGuide, std::vector<QtCharts::QLineSeries*>> _curvesPerGuide;
#else
  std::map<DataGuide, std::vector<QLineSeries*>> _curvesPerGuide;
#endif

// // // // //   QwtPlotPicker*   _picker;

protected:

                   PlotVisualisation   (DataObject* object,
                                        const std::string& visualisationName,
                                        QWidget* parent=nullptr,
                                        const char* name=nullptr);

  void             enableMarker        (long int marker);

  void             disableMarker       (long int marker);

  bool             markerEnabled       (long int marker) const;

  long int         xMarker             () const;

  long int         yMarker             () const;

  void             setXMarker          (double value);

  void             setYMarker          (double value);

  void             attachMarkers       ();

  void             detachMarkers       ();

  void             clearPlot           ();

  void             trackClickPoint     ();

  void             trackDragPoint      ();

  void             trackDragRect       ();

  void             drawCurve           (DataGuide const& guide,
                                        double* x,
                                        double* y,
                                        size_t nrValues,
                                        QPen const& pen);

  bool             intersectMarker     (double* x,
                                        double* y,
                                        long int marker,
                                        DataGuide const& guide) const;

  bool             onlyCumulativeProbabilitiesShown() const;

  bool             onlyExceedanceProbabilitiesShown() const;


#if QT_VERSION < QT_VERSION_CHECK(6, 0, 0)
  QtCharts::QChart *m_chart;

  QtCharts::QValueAxis *m_axisX;

  QtCharts::QValueAxis *m_axisY;
#else
  QChart *m_chart;

  QValueAxis *m_axisX;

  QValueAxis *m_axisY;
#endif

  void             mousePressEvent(QMouseEvent *event) override;

  void             mouseMoveEvent(QMouseEvent *event) override;



protected Q_SLOTS:

  virtual void     selected            (QPointF const& point);

  virtual void     selected            (QRectF const& rect);

  virtual void     selected            (const QVector<QPointF>& array);

  virtual void     appended            (QPointF const& point);

  virtual void     moved               (QPointF const& point);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   PlotVisualisation   (PlotVisualisation const& other) = delete;

  PlotVisualisation& operator=         (PlotVisualisation const& other) = delete;

           ~PlotVisualisation  () override;

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  bool             close               () override;

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  QPixmap          pixmap              ();

};



//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace ag

#endif

