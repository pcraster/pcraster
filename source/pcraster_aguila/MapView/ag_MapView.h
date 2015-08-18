#ifndef INCLUDED_AG_MAPVIEW
#define INCLUDED_AG_MAPVIEW



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_AG_BUFFEREDWIDGET
#include "ag_BufferedWidget.h"
#define INCLUDED_AG_BUFFEREDWIDGET
#endif

// PCRaster library headers.

// Module headers.



namespace ag {
  // MapView declarations.
}



namespace ag {

//! short_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
/*!
  longer_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED

  \sa        .
*/
class MapView: public BufferedWidget
{

  friend class MapViewTest;

private:

  QRectF           d_extent;

  QTransform       d_worldTransform;

  void             initializeWidget    ();

  bool             extentIsValid       () const;

  double           scale               () const;

protected:

  virtual void     updateBuffer        (QRect const& area);

  QRectF const&    extent              () const;

  void             zoomBy              (double factor);

  // void             zoomTo              (QPoint const& position,
  //                                       qreal factor);

  void             focusOn             (QPoint const& position);

  QTransform const& worldTransform     () const;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   MapView             (Alignment alignment,
                                        QWidget* parent=0);

  virtual          ~MapView            ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void             initializeExtent    (QRectF const& extent);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

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
