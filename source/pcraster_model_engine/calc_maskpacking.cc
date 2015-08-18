#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_MASKPACKING
#include "calc_maskpacking.h"
#define INCLUDED_CALC_MASKPACKING
#endif

// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_CALC_SPATIAL
#include "calc_spatial.h"
#define INCLUDED_CALC_SPATIAL
#endif

#ifndef INCLUDED_CALC_CR
#include "calc_cr.h"
#define INCLUDED_CALC_CR
#endif

#ifndef INCLUDED_COM_CSFCELL
#include "com_csfcell.h"
#define INCLUDED_COM_CSFCELL
#endif

/*!
  \file
  This file contains the implementation of the MaskPacking class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC MASKPACKING MEMBERS
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// DEFINITION OF MASKPACKING MEMBERS
//------------------------------------------------------------------------------

/*!
 * \param mask array of rs.nrCells() elements,0 means MV, 1 means value in compressed buffer
 *
 */
calc::MaskPacking::MaskPacking(
    const geo::RasterDim&    rs,
    const Mask&              mask):
        SpatialPacking(rs),
        d_compressedToDecompressed(rs.nrCells()),
        d_decompressedToCompressed(rs.nrCells()),
        d_rlIndex(rs.nrCells())
{
  PRECOND(mask.size()==rs.nrCells());
  size_t compressedSize(0);

  d_evenIsValueRL =mask[0]==1;
  if (mask[0]==1) {
    d_decompressedToCompressed[0]= compressedSize;
    d_compressedToDecompressed[compressedSize++]=0;
  } else {
      d_decompressedToCompressed[0]=invalidId();
      // no index in compressed representation
  }

  d_rlIndex[0]=0;
  d_rlIndex[1]=1;
  size_t rlInd=1;

  for(size_t i=1; i < rs.nrCells(); i++) {
    if (mask[i]==1) {
      d_decompressedToCompressed[i]=compressedSize;
      d_compressedToDecompressed[compressedSize++]=i;
    } else {
      d_decompressedToCompressed[i]=invalidId();
      // no index in compressed representation
    }
    if ((mask[i]==1) != (mask[i-1]==1))
    {
      d_rlIndex[rlInd+1]= d_rlIndex[rlInd];
      rlInd++;
    }
    d_rlIndex[rlInd]++;
  }
  d_compressedToDecompressed.resize(compressedSize);

  d_rlIndex.resize(rlInd+1);

#ifdef DEBUG
  // SANITY CHECK

  // last points to past-the-end
  POSTCOND(d_rlIndex.back()==rs.nrCells());
  // should have at least begin and end
  POSTCOND(d_rlIndex.size()>=2);

  PRECOND(rs.nrCells() == d_decompressedToCompressed.size());


  for(size_t fieldId=0; fieldId < d_compressedToDecompressed.size(); ++fieldId) {
    size_t rId= toRasterId(fieldId);
    POSTCOND(rId < rs.nrCells());
    PRECOND(mask[rId]==1);
    PRECOND(toFieldId(rId)==fieldId);
  }
  {
    size_t nrInMask=0;
   for(size_t rasterId=0; rasterId < d_decompressedToCompressed.size(); ++rasterId) {
    if (toFieldId(rasterId) != invalidId()) {
      PRECOND(toRasterId(toFieldId(rasterId))==rasterId);
      nrInMask++;
    }
   }
   POSTCOND(nrInMask==d_compressedToDecompressed.size());
  }
#endif
}

calc::MaskPacking::~MaskPacking()
{
}


size_t calc::MaskPacking::toRasterId(size_t fieldId) const
{
  PRECOND(fieldId < nrFieldCells());
  return d_compressedToDecompressed[fieldId];
}

size_t calc::MaskPacking::toFieldId(size_t rasterId) const
{
  PRECOND(rasterId < d_decompressedToCompressed.size());
  return d_decompressedToCompressed[rasterId];
}


size_t  calc::MaskPacking::nrFieldCells() const
{
  return d_compressedToDecompressed.size();
}

//! creates a spatial with an unpacked version of \a f, caller must delete
const calc::Field* calc::MaskPacking::unpack(const Field* f) const
{
  Spatial *r = new Spatial(f->vs(),f->cri(),rasterDim().nrCells());

  switch(f->cri()) {
      case CRI_1: decompress<>(r->dest_1(),f->src_1()); break;
      case CRI_4: decompress<>(r->dest_4(),f->src_4()); break;
      case CRI_f: decompress<>(r->dest_f(),f->src_f()); break;
      default: PRECOND(FALSE);
  }
  return r;
}


calc::Field* calc::MaskPacking::createSpatial(VS vs)const
{
  return new Spatial(vs,CRI_X,nrFieldCells());
}

//! create Spatial with compressed contents, from unpacked \a f,caller must delete
/*!
 * \todo
 *   if f->vs() == VS_L check if unsound ldd is created. Here or at input?
 */
calc::Field* calc::MaskPacking::pack(const Field* f)const
{
  Spatial *s= new Spatial(f->vs(),f->cri(),nrFieldCells());
  switch(f->cri()) {
      case CRI_1: compress<>(s->dest_1(),f->src_1()); break;
      case CRI_4: compress<>(s->dest_4(),f->src_4()); break;
      case CRI_f: compress<>(s->dest_f(),f->src_f()); break;
      default: PRECOND(FALSE);
     }
  return s;
}

template<typename T>
void calc::MaskPacking::compress(T* dest, const T *src) const
{
 for(size_t i=0; i < d_rlIndex.size()-1; ++i) {
   size_t n= d_rlIndex[i+1]-d_rlIndex[i];
   if ( i%2/*==odd*/ != d_evenIsValueRL) {
    memcpy(dest, src,n*sizeof(T));
    dest+=n;
   }
   src+=n;
 }
}

template<typename T>
void calc::MaskPacking::decompress(T* dest, const T *src) const
{
 for(size_t i=0; i < d_rlIndex.size()-1; ++i) {
   size_t n= d_rlIndex[i+1]-d_rlIndex[i];
   if ( i%2/*==odd*/ != d_evenIsValueRL) {
    memcpy(dest, src,n*sizeof(T));
    src+=n;
   } else
    pcr::setMV(dest,n);
   dest+=n;
 }
}

calc::MaskPacking* calc::MaskPacking::createClone() const
{
  return new MaskPacking(*this);
}


//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



