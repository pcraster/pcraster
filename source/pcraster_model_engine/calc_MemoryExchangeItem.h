#ifndef INCLUDED_CALC_MEMORYEXCHANGEITEM
#define INCLUDED_CALC_MEMORYEXCHANGEITEM



// External headers.
#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif

// Project headers.

// Module headers.



namespace calc {
  // MemoryExchangeItem declarations.
}



namespace calc {

//! remembers per user request allocated values in MemoryExchange API
class MemoryExchangeItem
{

private:
  //! script symbol name
  std::string      d_name;

  //! index into user's DataTransferArray
  size_t           d_id;

protected:
                   MemoryExchangeItem               ();


public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------


                   MemoryExchangeItem               (std::string const& name,
                                                     size_t id);
  /* virtual */    ~MemoryExchangeItem              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  size_t        id                                  () const;
  std::string const& name                           () const;
  virtual void* rawValue                            () const;
  virtual void  beMemCpySrc                         (void *dest) const;
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
