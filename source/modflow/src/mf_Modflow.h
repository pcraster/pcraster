#ifndef INCLUDED_MODFLOW_MODFLOW
#define INCLUDED_MODFLOW_MODFLOW

#include "stddefx.h"



namespace mf {
  // Modflow declarations.
}



namespace mf {

//! short_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
/*!
  longer_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED

  \sa        .
*/
class Modflow
{

  friend class ModflowTest;

private:

protected:

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   Modflow               ();

                   Modflow               (const Modflow&) = delete;

  Modflow&         operator=             (const Modflow&) = delete;

  /* virtual */    ~Modflow              ();

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

} // namespace mf

#endif
