#ifndef INCLUDED_AG_FEATURELAYER
#define INCLUDED_AG_FEATURELAYER



// External headers.

// Project headers.
#include "dal_DataSpace.h"
#include "dal_FeatureLayer.h"

// Module headers.
#include "ag_SpatialDataset.h"



namespace dal {
  class SpaceDimensions;
}
namespace ag {
  // FeatureLayer declarations.
}



namespace ag {

//! short_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
/*!
  longer_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED

  \sa        .
*/
class FeatureLayer: public SpatialDataset
{

  friend class FeatureLayerTest;

private:

  dal::FeatureLayer* d_layer;

  dal::DataSpace   d_dataSpace;

  CSF_VS           d_valueScale;

  bool             isRead              (dal::DataSpaceAddress const& address) const;

protected:

public:

  /// typedef dal::FeatureLayer::const_iterator const_iterator;

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   FeatureLayer        (std::string const& name,
                                        dal::DataSpace const& space);

  /* virtual */    ~FeatureLayer       ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void             read                (dal::DataSpace const& space,
                                        dal::DataSpaceAddress const& address);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  CSF_VS           valueScale          () const;

  bool             isRead              () const;

  bool             isEmpty             () const;

  size_t           size                () const;

  dal::SpaceDimensions const& dimensions() const;

  /// const_iterator   begin               () const;

  /// const_iterator   end                 () const;

  template<class InsertIterator>
  void             featureIds          (dal::FeatureLayerGeometries::Box
                                            const& box,
                                        InsertIterator inserter) const;

  OGRGeometry const& geometry          (long int featureId) const;

  bool             hasAttribute        () const;

  template<typename T>
  void             value               (long int featureId,
                                        T& value) const;

  template<typename T>
  bool             value               (T& result,
                                        dal::DataSpace const& space,
                                        dal::DataSpaceAddress const& address) const;

};



//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------

template<class InsertIterator>
void FeatureLayer::featureIds(
         dal::FeatureLayerGeometries::Box const& box,
         InsertIterator inserter) const
{
  d_layer->featureIds(box, inserter);
}



template<typename T>
inline bool FeatureLayer::value(
         T& result,
         dal::DataSpace const& space,
         dal::DataSpaceAddress const& address) const
{
  pcr::setMV(result);

  if(Dataset::isRead(space, address)) {
    size_t index = space.indexOf(dal::Space);

    if(space.hasRaster()) {
      ++index;
      assert(space.dimension(index).meaning() == dal::Space);
      assert(space.dimension(index).discretisation() ==
         dal::BorderedDiscretisation);
    }

    // Check if the coordinates are valid. Not valid if cursor is outside
    // the spatial area of the data.
    if(address.isValid(index)) {
      dal::SpatialCoordinate const& spatialAddress(
         address.coordinate<dal::SpatialCoordinate>(index));
      d_layer->value(spatialAddress, result);
    }
  }

  return !pcr::isMV(result);
}



template<typename T>
inline void FeatureLayer::value(
         long int featureId,
         T& value) const
{
  d_layer->value<T>(featureId, value);
}



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace ag

#endif
