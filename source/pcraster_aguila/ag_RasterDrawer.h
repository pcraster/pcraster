#ifndef INCLUDED_AG_RASTERDRAWER
#define INCLUDED_AG_RASTERDRAWER



// External headers.

// Project headers.

// Module headers.
#include "ag_MapDrawer.h"


class QRect;
namespace ag {
  // RasterDrawer declarations.
  class RasterDataset;
}



namespace ag {

//! short_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
/*!
  longer_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED

  \sa        .
*/
class RasterDrawer: public MapDrawer
{

  friend class RasterDrawerTest;

private:

  RasterDataset const* _raster;

  void             draw                (QPainter& painter,
                                        QRectF const& dirtyMapAreaInPixels,
                                        QTransform const& world_to_screen,
                                        QTransform const& screen_to_world) const;

  virtual void     draw                (QPainter& painter,
                                        QRect const& indices,
                                        QTransform const& world_to_screen,
                                        QTransform const& screen_to_world) const=0;

//   void             draw2               (QPainter& painter,
//                                         QRect const& indices,
//                                         QwtScaleMap const& xMapper,
//                                         QwtScaleMap const& yMapper) const;

  template<typename T>
  void             drawCells           (QPainter& painter,
                                        QRect const& indices,
                                        QTransform const& world_to_screen,
                                        QTransform const& screen_to_world) const;

protected:

                   RasterDrawer        (dal::SpaceDimensions const& spaceDimensions,
                                        RasterDataset const* raster);

  // RasterDataset const& raster          () const;

  double           cellSizeInPixels    (QTransform const& mapper) const;

  size_t           nrCellsPerPixel     (QTransform const& mapper) const;

  virtual void     drawCells           (QPainter& painter,
                                        QRect const& indices,
                                        QTransform const& world_to_screen,
                                        QTransform const& screen_to_world) const;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  virtual          ~RasterDrawer      ();

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
