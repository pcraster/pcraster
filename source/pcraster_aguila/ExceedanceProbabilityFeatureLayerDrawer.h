#ifndef INCLUDED_EXCEEDANCEPROBABILITYFEATURELAYERDRAWER
#define INCLUDED_EXCEEDANCEPROBABILITYFEATURELAYERDRAWER



// External headers.

// Project headers.

// Module headers.
#include "ag_FeatureLayerDrawer.h"



namespace ag {
  // ExceedanceProbabilityFeatureLayerDrawer declarations.
  class RangeDrawProps;
}



namespace ag {

//! short_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
/*!
  longer_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED

  \sa        .
*/
class ExceedanceProbabilityFeatureLayerDrawer: public FeatureLayerDrawer
{

  friend class ExceedanceProbabilityFeatureLayerDrawerTest;

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

                   ExceedanceProbabilityFeatureLayerDrawer(
                                        FeatureLayer const* layer,
                                        dal::SpaceDimensions const& dimensions,
                                        RangeDrawProps const& drawProperties);

  /* virtual */    ~ExceedanceProbabilityFeatureLayerDrawer              ();

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
