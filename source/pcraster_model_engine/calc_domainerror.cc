#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_DOMAINERROR
#include "calc_domainerror.h"
#define INCLUDED_CALC_DOMAINERROR
#endif

// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_CALC_GLOBALLIBDEFS
#include "calc_globallibdefs.h"
#define INCLUDED_CALC_GLOBALLIBDEFS
#endif


/*!
  \file
  This file contains the implementation of the DomainError class.
*/



//------------------------------------------------------------------------------

/*
namespace calc {

class DomainErrorPrivate
{
public:

  DomainErrorPrivate()
  {
  }

  ~DomainErrorPrivate()
  {
  }

};

} // namespace calc
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC DOMAINERROR MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF DOMAINERROR MEMBERS
//------------------------------------------------------------------------------

calc::DomainError::DomainError(const std::string& argMsg):
 com::Exception(argMsg)
{
}

calc::DomainError::DomainError():
  com::Exception("Domain Error")
{
}

calc::DomainError::~DomainError()
{
}

//! Assignment operator.
calc::DomainError& calc::DomainError::operator=(const DomainError& rhs)
{
  if (this != &rhs) {
   ;
  }
  return *this;
}

//! Copy constructor.
calc::DomainError::DomainError(const DomainError& rhs):
  com::Exception(rhs)
{
}


//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

void calc::throwDomainErrorFromCalcLib()
{
  throw DomainError(getLibError());
}


