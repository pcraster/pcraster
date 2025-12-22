#ifndef INCLUDED_DAL_MEMORYTABLEDATA
#define INCLUDED_DAL_MEMORYTABLEDATA

#include "dal_DataSpace.h"
#include "dal_MemoryData.h"
#include "dal_Table.h"

#include <any>
#include <vector>


namespace dal {
  // MemoryTableData declarations.
}



namespace dal {



//! short_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
/*!
  longer_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED

  \todo     Put most administrative logic in the base class. This should
            probably be a template class which can then be used by all
            Memory*Data classes.
*/
class MemoryTableData: public MemoryData
{

  friend class MemoryTableDataTest;

private:

  //! Values for all dimensions of the data space in a hierarchical layout.
  std::vector<std::any> d_values;

  //! Data space of the data values.
  DataSpace        d_dataSpace;

  //! Assignment operator. NOT IMPLEMENTED.
  MemoryTableData& operator=           (MemoryTableData const& rhs);

  void             initialiseValues    (std::vector<std::any>& values);

  void             initialiseValues    (std::vector<std::any>& values,
                                        DataSpace space);

  // void             copy                (std::vector<boost::any> const& sourceValues,
  //                                       DataSpace space,
  //                                       std::vector<boost::any>& destinationValues);

  void             add                 (Table* table,
                                        std::vector<std::any>& values);

  void             add                 (Table* table,
                                        DataSpace space,
                                        DataSpaceAddress address,
                                        std::vector<std::any>& values);

  void             add                 (Table* table,
                                        DataSpaceAddress const& address);

  void             clear               (std::vector<std::any>& values);

  void             clear               (std::vector<std::any>& values,
                                        DataSpace space);

  bool             exists              (std::vector<std::any> const& values) const;

  bool             exists              (DataSpace space,
                                        DataSpaceAddress address,
                                        std::vector<std::any> const& values) const;

  Table const*     table               (std::vector<std::any>& values);

  Table const*     table               (std::vector<std::any>& values,
                                        DataSpace space,
                                        DataSpaceAddress address);

protected:

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   MemoryTableData     (DataSpace const& space,
                                        DataSpaceAddress const& address,
                                        Table* table);

                   MemoryTableData     (MemoryTableData const& rhs);

  /* virtual */    ~MemoryTableData    () override;

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void             clear               ();

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  DataSpace const& dataSpace           () const;

  bool             exists              (DataSpaceAddress const& address) const;

  Table const*     table               (DataSpaceAddress const& address);

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
