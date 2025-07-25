#include "stddefx.h"
#include "calc_dllcalc.h"
#include "appargs.h"

#include <string>

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
