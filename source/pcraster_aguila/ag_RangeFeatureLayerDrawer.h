#ifndef INCLUDED_AG_RANGEFEATURELAYERDRAWER
#define INCLUDED_AG_RANGEFEATURELAYERDRAWER



// External headers.

// Project headers.

// Module headers.
#include "ag_FeatureLayerDrawer.h"



namespace ag {
  // RangeFeatureLayerDrawer declarations.
  class RangeDrawProps;
}



namespace ag {

//! short_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
/*!
  longer_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED

  \sa        .
*/
class RangeFeatureLayerDrawer: public FeatureLayerDrawer
{

  friend class RangeFeatureLayerDrawerTest;

private:

  RangeDrawProps const& _drawProperties;

  void             draw                (QPainter& painter,
                                        long int featureId,
                                        QPainterPath const& path) const;

protected:

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   RangeFeatureLayerDrawer(
                                        FeatureLayer const* layer,
                                        dal::SpaceDimensions const& dimensions,
                                        RangeDrawProps const& drawProperties);

  /* virtual */    ~RangeFeatureLayerDrawer();

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
