#ifndef INCLUDED_DAL_BROWSEINFO
#define INCLUDED_DAL_BROWSEINFO



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



namespace dal {
  // BrowseInfo declarations.
}



namespace dal {

//! Class for objects containing information obtained during a browse.
/*!
  BrowseInfo objects contain the easy accessible information that is useful
  for clients browsing locations for attributes.

  Each attribute can be represented by a single BrowseInfo object, although
  the attribute may be stored in multiple files and/or data bases and/or
  whatever.

  \sa        .
*/
class PCR_DAL_DECL BrowseInfo
{

  friend class BrowseInfoTest;

private:

  //! Name of the attribute.
  std::string      _name;

 //! Data space the attribute was found in.
  DataSpace        _space;

  //! Data set type.
  DatasetType      _datasetType;

  //! Value type of the individual attribute values.
  TypeId           _typeId;

  //! Value scale of attribute.
  CSF_VS           _valueScale;

  //! Name of driver that recognized the attribute.
  std::string      _driverName;

protected:

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   BrowseInfo          ();

                   BrowseInfo          (std::string const& name,
                                        DataSpace const& space,
                                        DatasetType datasetType,
                                        TypeId typeId,
                                        CSF_VS valueScale,
                                        std::string const& driverName);

                   BrowseInfo          (BrowseInfo const& rhs);

  BrowseInfo&      operator=           (BrowseInfo const& rhs);

  /* virtual */    ~BrowseInfo         ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  std::string const& name              () const;

  DataSpace const& space               () const;

  DatasetType      datasetType         () const;

  TypeId           typeId              () const;

  CSF_VS           valueScale          () const;

  std::string const& driverName        () const;

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
