#ifndef INCLUDED_CALC_MEMORYEXCHANGEITEMSTRING
#define INCLUDED_CALC_MEMORYEXCHANGEITEMSTRING

#include "calc_MemoryExchangeItem.h"

#include <string>


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

  void*           rawValue() const override;
  void            beMemCpySrc(void *dest) const override;

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
