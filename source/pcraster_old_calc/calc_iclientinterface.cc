#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_ICLIENTINTERFACE
#include "calc_iclientinterface.h"
#define INCLUDED_CALC_ICLIENTINTERFACE
#endif

// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_CALC_CLIENTINTERFACE
#include "calc_clientinterface.h"
#define INCLUDED_CALC_CLIENTINTERFACE
#endif

#ifndef INCLUDED_CALC_CATCHALLEXCEPTIONS
#include "calc_catchallexceptions.h"
#define INCLUDED_CALC_CATCHALLEXCEPTIONS
#endif




/*!
  \file
  This file contains the implementation of the IClientInterface class.
*/



//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// DEFINITION OF STATIC ICLIENTINTERFACE MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF ICLIENTINTERFACE MEMBERS
//------------------------------------------------------------------------------

calc::IClientInterface::IClientInterface():
  d_ci(0)
{
  TRY_ALL {
    d_ci = new ClientInterface();
  } CATCH_ALL_EXCEPTIONS(d_errorStream);
}


calc::IClientInterface::~IClientInterface()
{
  if (d_ci) {
    delete d_ci;
    d_ci=0;
  }
}

void calc::IClientInterface::run()
{
  PRECOND(d_ci);
  d_ci->run();
  if (executeScriptStatus()==calc::ErrorExecScript) {
    d_errorStream<<d_ci->errorMsg();
  }
}

void calc::IClientInterface::setProgressCallBack(ProgressCallBack *pcb)
{
  TRY_ALL {
    d_ci->setProgressCallBack(pcb);
  } CATCH_ALL_EXCEPTIONS(d_errorStream);
}

void calc::IClientInterface::setRunDirectory(const com::PathName&  rd)
{
  TRY_ALL {
    d_ci->setRunDirectory(rd);
  } CATCH_ALL_EXCEPTIONS(d_errorStream);
}

void calc::IClientInterface::setScriptFile(const com::PathName& scriptName)
{
  TRY_ALL {
    d_ci->setScriptFile(scriptName);
  } CATCH_ALL_EXCEPTIONS(d_errorStream);
}

calc::ExecuteScriptStatus calc::IClientInterface::executeScriptStatus() const
{
  calc::ExecuteScriptStatus s;
  TRY_ALL {
    s=d_ci->executeScriptStatus();
  } CATCH_ALL_EXCEPTIONS(d_errorStream)
  return s;
}

bool calc::IClientInterface::error() const
{
  return !errorMessage().empty();
}

void calc::IClientInterface::setError(const char *msg)
{
  d_errorStream << msg;
}


const std::string&  calc::IClientInterface::errorMessage() const
{
  d_errorMsg=d_errorStream.str();
  return d_errorMsg;
}


//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------
