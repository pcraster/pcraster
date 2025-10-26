#ifndef INCLUDED_BLOCK_SANDCOMPACTOR
#define INCLUDED_BLOCK_SANDCOMPACTOR

#include "stddefx.h"
#include "pcrtypes.h"



namespace block {
  // SandCompactor declarations.
}



namespace block {



//! short_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
/*!
  A Mackey and Bridge compactor.
*/
class SandCompactor
{

  friend class SandCompactorTest;

private:

protected:

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   SandCompactor       ();

  /* virtual */    ~SandCompactor      ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  REAL4            operator()          (REAL4 originalThickness,
                                        REAL4 depth);

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
