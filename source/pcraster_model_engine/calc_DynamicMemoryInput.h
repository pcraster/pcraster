#ifndef INCLUDED_CALC_DYNAMICMEMORYINPUT
#define INCLUDED_CALC_DYNAMICMEMORYINPUT



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// External headers.
#ifndef INCLUDED_BOOST_NONCOPYABLE
#include <boost/noncopyable.hpp>
#define INCLUDED_BOOST_NONCOPYABLE
#endif

// Project headers.

// Module headers.
#ifndef INCLUDED_CALC_DATAVALUE
#include "calc_datavalue.h"
#define INCLUDED_CALC_DATAVALUE
#endif
#ifndef INCLUDED_CALC_DATATYPE
#include "calc_datatype.h"
#define INCLUDED_CALC_DATATYPE
#endif


namespace calc {
  // DynamicMemoryInput declarations.
  class DataTable;
  class IOStrategy;
}



namespace calc {

//! A Field that is present on disk under a fileName, where load() will read
class DynamicMemoryInput:
   public DataValue,
   public boost::noncopyable
{

private:

  size_t            d_memoryIndexId;
  DataType          d_dataType;
  DataTable const&  d_dataTable;
  IOStrategy const& d_ioStrategy;

protected:

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   DynamicMemoryInput               (size_t memoryIndexId,
                                                     DataType const& dataType,
                                                     DataTable const& dataTable,
                                                     IOStrategy const& ioStrategy);

  /* virtual */    ~DynamicMemoryInput              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  DataValue*        load                          ();

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  virtual OVS       ovs                            () const;

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

} // namespace calc

#endif
