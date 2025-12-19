#ifndef INCLUDED_CALC_DOMAINERROR
#define INCLUDED_CALC_DOMAINERROR

#include "stddefx.h"
#include "com_exception.h"
#include "pcraster_model_engine_export.h"



namespace calc {
  // DomainError declarations.
}



namespace calc {



//! marks an eror in operation inputs
class PCR_ME_EXPORT DomainError : public com::Exception
{

private:


public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   DomainError               ();

                   DomainError               (const std::string& msg);


  DomainError&           operator=           (const DomainError& rhs);

                   DomainError               (const DomainError& rhs);

  /* virtual */    ~DomainError              () override;

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

void throwDomainErrorFromCalcLib();


} // namespace calc

#endif
