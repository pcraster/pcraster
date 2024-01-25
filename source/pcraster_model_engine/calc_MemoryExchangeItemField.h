#ifndef INCLUDED_CALC_MEMORYEXCHANGEITEMFIELD
#define INCLUDED_CALC_MEMORYEXCHANGEITEMFIELD



// External headers.
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

#include  <memory>


namespace calc {
  // MemoryExchangeItemField declarations.
  class Field;
}



namespace calc {

//! Field version of MemoryExchangeItem
class MemoryExchangeItemField: public MemoryExchangeItem
{

private:
  std::shared_ptr<Field> d_value;

                   MemoryExchangeItemField               ();

protected:

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   MemoryExchangeItemField               (std::string const& name,
                                                          size_t memoryId,
                                                          std::shared_ptr<Field> value);

  /* virtual */    ~MemoryExchangeItemField              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  void*             rawValue                             () const override;
  void              beMemCpySrc                          (void *dest) const override;

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
