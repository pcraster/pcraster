#include "stddefx.h"
#include "calc_clientinterface.h"

/*!
  \file
  This file contains the implementation of the ClientInterface class.
*/


//------------------------------------------------------------------------------
// DEFINITION OF STATIC CLIENTINTERFACE MEMBERS
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// DEFINITION OF CLIENTINTERFACE MEMBERS
//------------------------------------------------------------------------------

//! ctor
calc::ClientInterface::ClientInterface()
{
}

//! dtor
calc::ClientInterface::~ClientInterface()
{
}

//! parse and execute script
int calc::ClientInterface::execute()
{
  // if exceptions are thrown then this
  // is the exit status
  d_executeScriptStatus = ErrorExecScript;

  parse();
  return executeScript();
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------
