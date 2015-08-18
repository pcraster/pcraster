#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_COM_ICOMMANDMODE
#include "com_icommandmode.h"
#define INCLUDED_COM_ICOMMANDMODE
#endif

// Library headers.

// PCRaster library headers.

// Module headers.



/*!
  \file
  This file contains the implementation of the ICommandMode class.
*/



//------------------------------------------------------------------------------

/*
namespace com {

class ICommandModePrivate
{
public:

  ICommandModePrivate()
  {
  }

  ~ICommandModePrivate()
  {
  }

};

} // namespace com
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC ICOMMANDMODE MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF ICOMMANDMODE MEMBERS
//------------------------------------------------------------------------------

//! Constructor.
/*!
*/
com::ICommandMode::ICommandMode()

  : LabeledProgressTracked<ProgressBar>()

{
}



//! Destructor.
/*!
*/
com::ICommandMode::~ICommandMode()
{
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



