#ifndef INCLUDED_CALC_RTTYPECHECK
#define INCLUDED_CALC_RTTYPECHECK

#include "stddefx.h"



namespace calc {
  // rtTypeCheck declarations.
  class Operator;
  class RunTimeEnv;
}



namespace calc {


//! short_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
/*!
  longer_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
*/
class RtTypeCheck
{

private:

  //! Assignment operator. NOT IMPLEMENTED.
  RtTypeCheck&           operator=           (RtTypeCheck const& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   RtTypeCheck               (RtTypeCheck const& rhs);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   RtTypeCheck               ();

  /* virtual */    ~RtTypeCheck              ();

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

void rtTypeCheck(
    const Operator&   op,
          RunTimeEnv* rte,
    size_t            nrActualArgs);

} // namespace calc

#endif
