#ifndef INCLUDED_AG_VECTORDRAWER
#define INCLUDED_AG_VECTORDRAWER



// External headers.

// Project headers.
#include "ag_RangeDrawProps.h"
#include "ag_RasterDrawer.h"

// Module headers.



namespace ag {
  // VectorDrawer declarations.
  class Vector;
}



namespace ag {

//! short_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
/*!
  longer_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED

  \sa        .
*/
class VectorDrawer: public RasterDrawer
{

  friend class VectorDrawerTest;

private:

  Vector const*    _vector;

  RangeDrawProps   _properties;

  void             draw                (QPainter& painter,
                                        QRect const& indices,
                                        QwtScaleMap const& xMapper,
                                        QwtScaleMap const& yMapper) const;

  template<typename T>
  void             drawCells           (QPainter& painter,
                                        QRect const& indices,
                                        QwtScaleMap const& xMapper,
                                        QwtScaleMap const& yMapper) const;

protected:

  void             drawCells           (QPainter& painter,
                                        QRect const& indices,
                                        QwtScaleMap const& xMapper,
                                        QwtScaleMap const& yMapper) const;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   VectorDrawer        (Vector const* vector,
                                        dal::SpaceDimensions const& dimensions,
                                        RangeDrawProps const& properties);

  /* virtual */    ~VectorDrawer       ();

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
