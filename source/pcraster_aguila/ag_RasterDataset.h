#ifndef INCLUDED_AG_RASTERDATASET
#define INCLUDED_AG_RASTERDATASET



// External headers.

// Project headers.

// Module headers.
#include "ag_SpatialDataset.h"



namespace ag {
  // RasterDataset declarations.
}



namespace ag {

//! short_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
/*!
  longer_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED

  \sa        .
*/
class RasterDataset: public SpatialDataset
{

  friend class RasterDatasetTest;

private:

protected:

                   RasterDataset       (std::string const& name,
                                        dal::DataSpace const& space);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  virtual          ~RasterDataset      ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  virtual dal::RasterDimensions const& dimensions() const=0;

  virtual dal::TypeId typeId           () const=0;

  virtual bool     isMV                (size_t row,
                                        size_t col) const=0;

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
