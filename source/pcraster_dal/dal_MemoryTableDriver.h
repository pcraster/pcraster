#ifndef INCLUDED_DAL_MEMORYTABLEDRIVER
#define INCLUDED_DAL_MEMORYTABLEDRIVER



// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_DAL_TABLEDRIVER
#include "dal_TableDriver.h"
#define INCLUDED_DAL_TABLEDRIVER
#endif



namespace dal {
  // MemoryTableDriver declarations.
  class MemoryDataPool;
}



namespace dal {



//! short_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
/*!
  longer_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
*/
class MemoryTableDriver: public TableDriver
{

  friend class MemoryTableDriverTest;

private:

  //! Data pool to store memory data in.
  MemoryDataPool* const d_dataPool;

  //! Assignment operator. NOT IMPLEMENTED.
  MemoryTableDriver& operator=         (MemoryTableDriver const& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   MemoryTableDriver   (MemoryTableDriver const& rhs);

protected:

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   MemoryTableDriver   (MemoryDataPool* dataPool);

  /* virtual */    ~MemoryTableDriver  ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  bool             exists              (std::string const& name,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address) const;

  Table*           open                (std::string const& name,
                                        dal::DataSpace const& space,
                                        dal::DataSpaceAddress const& address) const;

  Table*           read                (std::string const& name,
                                        dal::DataSpace const& space,
                                        dal::DataSpaceAddress const& address) const;

  void             read                (Table& table,
                                        std::string const& name,
                                        dal::DataSpace const& space,
                                        dal::DataSpaceAddress const& address) const;

  void             write               (Table const& table,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address,
                                        std::string const& name) const;

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
