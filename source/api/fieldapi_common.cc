#include "stddefx.h"
#include "fieldapi_common.h"
#include "geo_cellloc.h"



/*!
  \file
  This file contains the implementation of the Common class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC COMMON MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF COMMON MEMBERS
//------------------------------------------------------------------------------

//! ctor
/*!
    \todo
     if nrRows and nrCols are 0 then init with global known dimensions
 */
fieldapi::Common::Common(size_t nrRows,size_t nrCols):
  RasterDim(nrRows,nrCols)
{
}

//! dtor
fieldapi::Common::~Common()
{
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



