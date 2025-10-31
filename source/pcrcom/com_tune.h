#ifndef INCLUDED_COM_TUNE
#define INCLUDED_COM_TUNE

#include "stddefx.h"



namespace com {
  // Tune declarations.
}



namespace com {



//! short_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
/*!
  longer_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
*/
class Tune
{

  friend class TuneTest;

private:

  //! Assignment operator. NOT IMPLEMENTED.
  Tune&           operator=           (Tune const& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   Tune               (Tune const& rhs);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   Tune               ();

  /* virtual */    ~Tune              ();

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

void tune();

} // namespace com

#endif
