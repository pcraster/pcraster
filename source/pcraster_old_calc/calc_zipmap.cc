#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_ZIPMAP
#include "calc_zipmap.h"
#define INCLUDED_CALC_ZIPMAP
#endif

// Library headers.
#ifndef INCLUDED_ZLIB
#include <zlib.h>
#define INCLUDED_ZLIB
#endif

#ifndef INCLUDED_CSTRING
#include <cstring> // memset
#define INCLUDED_CSTRING
#endif
// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_CALC_MAP2CSF
#include "calc_map2csf.h"
#define INCLUDED_CALC_MAP2CSF
#endif



/*!
  \file
  This file contains the implementation of the ZipMap class.
*/

//------------------------------------------------------------------------------
// DEFINITION OF STATIC ZIPMAP MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF ZIPMAP MEMBERS
//------------------------------------------------------------------------------

//! initialise with zipped data of \a f
calc::ZipMap::ZipMap(const Spatial *f):
 Spatial(f->vs(),f->nrValues(), false)
{

 // shifting 3 bits adds 12.5 % to size, 10 % needed
 d_nrZlibCompressedData=valLen()+(valLen()>>3);
 DEVELOP_PRECOND(d_nrZlibCompressedData >= (valLen()*1.1));
 char *cBuf = new char[d_nrZlibCompressedData];

 DEVELOP_PRECOND(sizeof(size_t) == sizeof(uLongf));
 compress2((Bytef *)cBuf,(uLongf *)&d_nrZlibCompressedData,
           (const Bytef *)f->srcValue(),valLen(),1);
 d_zlibCompressedData = new char[d_nrZlibCompressedData];
 memcpy(d_zlibCompressedData,cBuf,d_nrZlibCompressedData);
 delete [] cBuf;
}

calc::ZipMap::~ZipMap()
{
 delete [] d_zlibCompressedData;
 d_zlibCompressedData=0;
}

void calc::ZipMap::loadExternal() const
{
  allocate();
  uLongf uncomprLen=valLen();
  uncompress((Bytef *)valuePtr(),&uncomprLen,
             (const Bytef *)d_zlibCompressedData,d_nrZlibCompressedData);
  POSTCOND(uncomprLen==valLen());
  delete [] d_zlibCompressedData;
  d_zlibCompressedData=0;
}

calc::ZipMap::ZipMap(const ZipMap &c):
 Spatial(c.vs(),c.nrValues(),false)
{
 d_nrZlibCompressedData = c.d_nrZlibCompressedData;
 d_zlibCompressedData = new char[d_nrZlibCompressedData];
 memcpy(d_zlibCompressedData,c.d_zlibCompressedData,d_nrZlibCompressedData);
}

calc::ZipMap *calc::ZipMap::copy() const
{
 if(!d_zlibCompressedData) {
   if (valuePtr()) {
     // copy spatial into a zipped one
      return new ZipMap(this);
   }
   PRECOND(valuePtr() || d_zlibCompressedData);
 }
 return new ZipMap(*this);
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------
