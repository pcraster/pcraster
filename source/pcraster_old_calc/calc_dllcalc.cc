#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_DLLCALC
#include "calc_dllcalc.h"
#define INCLUDED_CALC_DLLCALC
#endif

// Library headers.
#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif

// PCRaster library headers.
#ifndef INCLUDED_APPARGS
#include "appargs.h"
#define INCLUDED_APPARGS
#endif

// Module headers.

/*!
  \file
  This file contains the implementation of DllCalc class
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC DLLCALC MEMBERS
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// DEFINITION OF DLLCALC MEMBERS
//------------------------------------------------------------------------------

//! ctor
calc::DllCalc::DllCalc():
  Calc(d_devNull,d_error)
{
 appOutput=APP_NOOUT;
}

//! return the error Msg set in execute(), empty string if no error
std::string calc::DllCalc::errorMsg() const
{
 return d_error.str();
}

//! dtor
calc::DllCalc::~DllCalc()
{
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------
