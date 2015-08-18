#ifndef INCLUDED_BLOCK_NEWCLASS
#define INCLUDED_BLOCK_NEWCLASS



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.

// Module headers.



namespace block {
  // NewClass declarations.
}



namespace block {

//! short_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
/*!
  longer_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
*/
class NewClass
{

  friend class NewClassTest;

private:

  //! Assignment operator. NOT IMPLEMENTED.
  NewClass&           operator=           (NewClass const& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   NewClass               (NewClass const& rhs);

protected:

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   NewClass               ();

  /* virtual */    ~NewClass              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

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

} // namespace block

#endif
