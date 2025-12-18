#ifndef INCLUDED_AG_RASTERDATASOURCES
#define INCLUDED_AG_RASTERDATASOURCES

#include "ag_DataObjectBase.h"
#include "ag_Raster.h"



namespace ag {
  // RasterDataSources declarations.
}



namespace ag {



//! short_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
/*!
  longer_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
*/
class RasterDataSources: public DataObjectBase<Raster>
{

  friend class RasterDataSourcesTest;

private:

  DataInfo<Raster> openData            (std::string const& name,
                                        dal::DataSpace const& space) const override;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   RasterDataSources   ();

  /* virtual */    ~RasterDataSources  () override;

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
