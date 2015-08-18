#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_DECOMPRESSEDDATA
#include "calc_decompresseddata.h"
#define INCLUDED_CALC_DECOMPRESSEDDATA
#endif

// Library headers.

// PCRaster library headers.

// Module headers.



/*!
  \file
  This file contains the implementation of the DecompressedData class.
*/


//------------------------------------------------------------------------------
// DEFINITION OF STATIC DECOMPRESSEDDATA MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF DECOMPRESSEDDATA MEMBERS
//------------------------------------------------------------------------------

calc::DecompressedData::DecompressedData(VS vs):
      d_vs(vs),
      d_originalData(0),
      d_decompressedCopy(0)
{
}

calc::DecompressedData::~DecompressedData()
{
   delete [] d_decompressedCopy;
}

//! originalData is the decompressed version
void calc::DecompressedData::setOriginalData(const void *originalData)
{
   PRECOND(!d_decompressedCopy);
   d_originalData=originalData;
}

//! the copy is decompressed version
void calc::DecompressedData::setDecompressedCopy(const unsigned char *decompressedCopy)
{
   PRECOND(!d_originalData);
   d_decompressedCopy=decompressedCopy;
}

//! return ptr to buffer with decompressed data
const void *calc::DecompressedData::decompressed() const
{
   if (d_originalData)
     return d_originalData;
   return (const void *)d_decompressedCopy;
}


//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



