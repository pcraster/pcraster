#ifndef INCLUDED_AG_TESTMAPVIEW
#define INCLUDED_AG_TESTMAPVIEW



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_AG_MAPVIEW
#include "ag_MapView.h"
#define INCLUDED_AG_MAPVIEW
#endif



namespace ag {
  // TestMapView declarations.
}



namespace ag {

//! short_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
/*!
  longer_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED

  \sa        .
*/
class TestMapView: public MapView
{

  friend class TestMapViewTest;

private:

  void             updateBuffer        (QRect const& area);

  void             mousePressEvent     (QMouseEvent* event);

  void             mouseMoveEvent      (QMouseEvent* event);

  void             mouseReleaseEvent   (QMouseEvent* event);

  void             mouseDoubleClickEvent(QMouseEvent* event);

protected:

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   TestMapView         (Alignment alignment,
                                        QWidget* parent=0);

  /* virtual */    ~TestMapView        ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

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
