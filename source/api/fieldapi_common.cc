#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_FIELDAPI_COMMON
#include "fieldapi_common.h"
#define INCLUDED_FIELDAPI_COMMON
#endif

// Library headers.

// PCRaster library headers.
#ifndef INCLUDED_GEO_CELLLOC
#include "geo_cellloc.h"
#define INCLUDED_GEO_CELLLOC
#endif

// Module headers.



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



