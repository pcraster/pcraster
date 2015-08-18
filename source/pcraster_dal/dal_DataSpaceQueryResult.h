#ifndef INCLUDED_DAL_DATASPACEQUERYRESULT
#define INCLUDED_DAL_DATASPACEQUERYRESULT



// External headers.

// Project headers.

// Module headers.
#ifndef INCLUDED_DAL_CONFIGURE
#include "dal_Configure.h"
#define INCLUDED_DAL_CONFIGURE
#endif

#ifndef INCLUDED_DAL_DATASPACE
#include "dal_DataSpace.h"
#define INCLUDED_DAL_DATASPACE
#endif

#ifndef INCLUDED_DAL_DATASPACEADDRESS
#include "dal_DataSpaceAddress.h"
#define INCLUDED_DAL_DATASPACEADDRESS
#endif



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

  DatasetType      d_datasetType;

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
