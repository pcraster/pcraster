#ifndef INCLUDED_AG_MAPDRAWER
#define INCLUDED_AG_MAPDRAWER



// External headers.
#include <boost/noncopyable.hpp>
#include <boost/tuple/tuple.hpp>
#include <QTransform>

// Project headers.
#include "dal_SpaceDimensions.h"

// Module headers.



class QPainter;
class QPointF;
class QPoint;
class QRectF;

namespace ag {
  // MapDrawer declarations.
}


namespace ag {

//! Base class for map drawers.
/*!
  This class contains some code map drawers have in common.

  \sa        .
  \todo      Rename to MapDrawer.
*/
class MapDrawer: private boost::noncopyable
{

  friend class MapDrawerTest;

private:

  //! Spatial dimensions of area to draw map in.
  dal::SpaceDimensions _overallDimensions;

  dal::SpaceDimensions _attributeDimensions;

  virtual void     draw                (QPainter& painter,
                                        QRectF const& dirtyMapAreaInPixels,
                                        QTransform const& world_to_screen,
                                        QTransform const& screen_to_world
                                       ) const=0;

protected:

                   MapDrawer           (dal::SpaceDimensions const& overallDimensions,
                                        dal::SpaceDimensions const& attribibuteDimensions);

  QRectF           envelopeInPixels    (QPointF const& anchor,
                                        double zoom,
                                        QPointF const& offset,
                                        double scale) const;

  boost::tuple<QTransform, QTransform> mappers(
                                        QRectF const& envelopeInPixels) const;

  double           scale               (QTransform const& mapper) const;

//   void             drawOutline         (QPainter& painter,
//                                         QwtScaleMap const& xMapper,
//                                         QwtScaleMap const& yMapper) const;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  virtual          ~MapDrawer          ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  void             draw                (QPainter& painter,
                                        QRectF const& dirtyScreenArea,
                                        QPointF const& anchor,
                                        double zoom,
                                        QPointF const& offset,
                                        double scale);

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
