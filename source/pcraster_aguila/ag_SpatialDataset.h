#ifndef INCLUDED_AG_SPATIALDATASET
#define INCLUDED_AG_SPATIALDATASET



// External headers.

// Project headers.

// Module headers.
#include "ag_Dataset.h"



namespace ag {
  // SpatialDataset declarations.
}



namespace ag {

//! short_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
/*!
  longer_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED

  \sa        .
*/
class SpatialDataset: public Dataset
{

  friend class SpatialDatasetTest;

private:

protected:

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   SpatialDataset      (std::string const& name,
                                        dal::DataSpace const& space);

  virtual          ~SpatialDataset     ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void             readTimeSeries      (dal::DataSpace const& space,
                                        dal::DataSpaceAddress const& address,
                                        dal::Table& table);

  void             readCumulativeProbabilities(
                                        dal::DataSpace const& space,
                                        dal::DataSpaceAddress const& address,
                                        dal::Table& table);

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
