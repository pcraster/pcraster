#ifndef INCLUDED_CALC_MEMORYEXCHANGEITEMFIELD
#define INCLUDED_CALC_MEMORYEXCHANGEITEMFIELD



// External headers.
#ifndef INCLUDED_BOOST_SHARED_PTR
#include <boost/shared_ptr.hpp>
#define INCLUDED_BOOST_SHARED_PTR
#endif
#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif
// Project headers.

// Module headers.
#ifndef INCLUDED_CALC_MEMORYEXCHANGEITEM
#include "calc_MemoryExchangeItem.h"
#define INCLUDED_CALC_MEMORYEXCHANGEITEM
#endif



namespace calc {
  // MemoryExchangeItemField declarations.
  class Field;
}



namespace calc {

//! Field version of MemoryExchangeItem
class MemoryExchangeItemField: public MemoryExchangeItem
{

private:
  boost::shared_ptr<Field> d_value;

                   MemoryExchangeItemField               ();

protected:

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   MemoryExchangeItemField               (std::string const& name,
                                                          size_t memoryId,
                                                          boost::shared_ptr<Field> value);

  /* virtual */    ~MemoryExchangeItemField              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  void*             rawValue                             () const;
  void              beMemCpySrc                          (void *dest) const;

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
