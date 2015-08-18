#ifndef INCLUDED_AG_FEATUREDATASOURCES
#define INCLUDED_AG_FEATUREDATASOURCES



// External headers.

// Project headers.

// Module headers.
#include "ag_DataObjectBase.h"
#include "ag_FeatureLayer.h"



namespace ag {
  // FeatureDataSources declarations.
}



namespace ag {

//! short_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
/*!
  longer_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED

  \sa        .
*/
class FeatureDataSources: public DataObjectBase<FeatureLayer>
{

  friend class FeatureDataSourcesTest;

private:

  DataInfo<FeatureLayer> openData      (std::string const& name,
                                        dal::DataSpace const& space) const;

protected:

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   FeatureDataSources  ();

  /* virtual */    ~FeatureDataSources ();

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
