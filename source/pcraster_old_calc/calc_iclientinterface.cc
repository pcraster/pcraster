#include "stddefx.h"
#include "calc_iclientinterface.h"
#include "calc_clientinterface.h"
#include "calc_catchallexceptions.h"

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

calc::IClientInterface::IClientInterface()

{
  TRY_ALL
  {
    d_ci = new ClientInterface();
  }
  CATCH_ALL_EXCEPTIONS(d_errorStream);
}

calc::IClientInterface::~IClientInterface()
{
  if (d_ci != nullptr) {
    delete d_ci;
    d_ci = nullptr;
  }
}

void calc::IClientInterface::run()
{
  PRECOND(d_ci);
  d_ci->run();
  if (executeScriptStatus() == calc::ErrorExecScript) {
    d_errorStream << d_ci->errorMsg();
  }
}

void calc::IClientInterface::setProgressCallBack(ProgressCallBack *pcb)
{
  TRY_ALL
  {
    d_ci->setProgressCallBack(pcb);
  }
  CATCH_ALL_EXCEPTIONS(d_errorStream);
}

void calc::IClientInterface::setRunDirectory(const com::PathName &rd)
{
  TRY_ALL
  {
    d_ci->setRunDirectory(rd);
  }
  CATCH_ALL_EXCEPTIONS(d_errorStream);
}

void calc::IClientInterface::setScriptFile(const com::PathName &scriptName)
{
  TRY_ALL
  {
    d_ci->setScriptFile(scriptName);
  }
  CATCH_ALL_EXCEPTIONS(d_errorStream);
}

calc::ExecuteScriptStatus calc::IClientInterface::executeScriptStatus() const
{
  calc::ExecuteScriptStatus s{};
  TRY_ALL
  {
    s = d_ci->executeScriptStatus();
  }
  CATCH_ALL_EXCEPTIONS(d_errorStream)
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

const std::string &calc::IClientInterface::errorMessage() const
{
  d_errorMsg = d_errorStream.str();
  return d_errorMsg;
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------
