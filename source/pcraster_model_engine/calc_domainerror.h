#ifndef INCLUDED_CALC_DOMAINERROR
#define INCLUDED_CALC_DOMAINERROR



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif

// PCRaster library headers.

// Module headers.



namespace calc {
  // DomainError declarations.
}



namespace calc {



//! marks an eror in operation inputs
class DomainError : public com::Exception
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

  /* virtual */    ~DomainError              ();

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
