#ifndef INCLUDED_DAL_DATASPACEQUERYRESULT
#define INCLUDED_DAL_DATASPACEQUERYRESULT

#include "dal_Configure.h"
#include "dal_DataSpace.h"
#include "dal_DataSpaceAddress.h"



namespace dal {
  // DataSpaceQueryResult declarations.
}



namespace dal {

//! short_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
/*!
  longer_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED

  \sa        .
*/
class PCR_DAL_DECL DataSpaceQueryResult
{

  friend class DataSpaceQueryResultTest;

private:

  std::string      d_name;

  DatasetType      d_datasetType{NR_DATASET_TYPES};

  //! Data space corresponding to addresses at which data was found / enclosing data space.
  DataSpace        d_space;

  //! First address at which data is present.
  DataSpaceAddress d_address;

protected:

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   DataSpaceQueryResult();

                   DataSpaceQueryResult(std::string const& name,
                                        DatasetType datasetType,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address);

  /* virtual */    ~DataSpaceQueryResult();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

                   operator bool       () const;

  std::string const& name              () const;

  DatasetType      datasetType         () const;

  DataSpace const& space               () const;

  DataSpaceAddress const& address      () const;

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

} // namespace dal

#endif
