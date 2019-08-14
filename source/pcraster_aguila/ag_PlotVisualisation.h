#ifndef INCLUDED_AG_PLOTVISUALISATION
#define INCLUDED_AG_PLOTVISUALISATION



// Library headers.
#include <map>
#include <boost/noncopyable.hpp>
#include <QtCharts/QChartView>
#include <QtCharts/QLineSeries>
#include <QtCharts/QValueAxis>
#include <QtWidgets/QGraphicsView>

// PCRaster library headers.
#include "ag_DataGuide.h"
#include "ag_IVisualisation.h"
#include "ag_LineMarker.h"

// Module headers.



namespace ag {
  // PlotVisualisation declarations.
  class DataObject;
}


namespace ag {



//! short_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
/*!
  longer_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
*/
class PlotVisualisation: public QtCharts::QChartView,
                         public ag::IVisualisation,
                         private boost::noncopyable
{

private:

  Q_OBJECT

  LineMarker*       _xMarker;

  //LineMarker*       _yMarker;

  long int         _xMarkerId;

  bool             _xMarkerEnabled;

  long int         _yMarkerId;

  bool             _yMarkerEnabled;

  long int         _selectedMarkerId;

  std::map<DataGuide, std::vector<QtCharts::QLineSeries*>> _curvesPerGuide;

// // // // //   QwtPlotPicker*   _picker;

protected:

                   PlotVisualisation   (DataObject* object,
                                        const std::string& visualisationName,
                                        QWidget* parent=0,
                                        const char* name=0);

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

  QtCharts::QChart *m_chart;

  QtCharts::QValueAxis *m_axisX;

  QtCharts::QValueAxis *m_axisY;

  void             mousePressEvent(QMouseEvent *event);

  void             mouseMoveEvent(QMouseEvent *event);



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

  virtual          ~PlotVisualisation  ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  bool             close               ();

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

