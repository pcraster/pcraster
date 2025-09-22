#ifndef INCLUDED_AG_MAP2DVIEW
#define INCLUDED_AG_MAP2DVIEW



// Library headers.
#include <vector>

// PCRaster library headers.

// Module headers.
#include "ag_BufferedVisualisation.h"
#include "ag_DataGuide.h"
#include "ag_Types.h"
#include "ag_MouseTarget.h"



namespace dal {
  class SpaceDimensions;
}
namespace ag {
  class DataObject;
}



namespace ag {



//! short_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
/*!
  longer_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
*/
class Map2DView: public ag::BufferedVisualisation
{

public:

private:

  Q_OBJECT

  // std::vector<Mode> _modes;

  // double           _scale;

  MouseTarget      _mapViewMouseTarget;

  enum Action {
    NoAction,
    Query,
    Pan,
    ZoomByRectangle
  };

  Action           _action{NoAction};

  //! Assignment operator. NOT IMPLEMENTED.
  Map2DView&       operator=           (const Map2DView&);

  //! Copy constructor. NOT IMPLEMENTED.
                   Map2DView           (const Map2DView&);

  // Mode             mode                () const;

  /// void             setMapAction        (MapAction action);

  // void             saveMode            ();

  // void             restoreMode         ();

  void             drawCrossHair       ();

  void             drawZoomRectangle   (QRect const& rectangle);

  // void             setScale            (double scale);

  // void             scaleBy             (double factor);

  double           pixelsToWorldUnits  (double amount) const;

  QPointF          pixelsToWorldUnits  (QPointF const& amount) const;

  double           worldUnitsToPixels  (double amount) const;

  QPointF          worldUnitsToPixels  (QPointF const& amount) const;

  void             queryMap            (QPoint const& pos);

  bool             map                 (double x,
                                        double y,
                                        QPointF& pos) const;

  bool             map                 (const QPointF& position,
                                        double* x,
                                        double* y) const;

  QRect            zoomRectangle       () const;

  void             zoomByRectangle     (QRect const& rectangle) const;

protected:

  void             paintEvent          (QPaintEvent* event) override;

  void             updateBuffer        (QRectF const& area) override;

  void             mousePressEvent     (QMouseEvent* event) override;

  void             mouseReleaseEvent   (QMouseEvent* event) override;

  void             mouseDoubleClickEvent(QMouseEvent* event) override;

  void             mouseMoveEvent      (QMouseEvent* event) override;

  void             wheelEvent          (QWheelEvent* event) override;

  void             keyPressEvent       (QKeyEvent* event) override;

  void             keyReleaseEvent     (QKeyEvent* event) override;

  // void             showEvent           (QShowEvent* event);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   Map2DView           (DataObject* object,
                                        QWidget* parent = nullptr);

  /* virtual */    ~Map2DView          () override;

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void             rescan              () override;

  void             process             () override;

  void             visualise           () override;

  void             addAttribute        (const DataGuide& dataGuide);

  void             clear               ();

  void             deleteScene         (QRectF const& area);

  void             createScene         (std::vector<DataGuide> const& guides,
                                        QRectF const& area);

  void             zoomAll             ();

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  double           scale               () const;

public Q_SLOTS:

  void             startQueryMode      ();

  void             startPanMode        ();

  void             startZoomAreaMode   ();

  void             startSelectMode     ();

  void             resetMapView        ();

Q_SIGNALS:

  void             dirty               (const QRect& area);

};



//------------------------------------------------------------------------------
// INLINE FUNCIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace ag

#endif
