#ifndef INCLUDED_BLOCK_CLAYCOMPACTOR
#define INCLUDED_BLOCK_CLAYCOMPACTOR

#include "stddefx.h"
#include "pcrtypes.h"



namespace block {
  // ClayCompactor declarations.
}



namespace block {

//! short_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
/*!
  A Mackey and Bridge compactor.
*/
class ClayCompactor
{

  friend class ClayCompactorTest;

private:

protected:

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   ClayCompactor       ();

  /* virtual */    ~ClayCompactor      ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  REAL4            operator()          (REAL4 originalThickness,
                                        REAL4 depth) const;

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
