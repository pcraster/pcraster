#ifndef INCLUDED_CALC_MEMORYEXCHANGEITEMSTRING
#define INCLUDED_CALC_MEMORYEXCHANGEITEMSTRING



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



namespace calc {
  // MemoryExchangeItemString declarations.
}



namespace calc {

//! string version of MemoryExchangeItem
class MemoryExchangeItemString: public MemoryExchangeItem
{

private:
  std::string      d_value;

                   MemoryExchangeItemString               ();

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   MemoryExchangeItemString             (
                                                         std::string const& name,
                                                         size_t memoryId,
                                                         std::string const& value);

  /* virtual */    ~MemoryExchangeItemString              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  void*           rawValue() const;
  void            beMemCpySrc(void *dest) const;

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
