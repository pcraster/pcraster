
/*! \file
 * the "irregular" cases typed in manual (not generated)
 */

#ifndef INCLUDED_CALC_IFOPOINTARRAY
#include "calc_ifopointarray.h"
#define INCLUDED_CALC_IFOPOINTARRAY
#endif
#ifndef INCLUDED_COM_CSFCELL
#include "com_csfcell.h"
#define INCLUDED_COM_CSFCELL
#endif

namespace calc {


 template<typename T>
  struct IDiffUnSingleType: public IDiffUn {
   CRIndex cri() const {
       return crIndex<T>();
   }
  };

 template<typename T>
  struct DefinedArray: public IDiffUnSingleType<T> {
   DefinedArray() {
       this->f=(IDiffUn::F)fImpl;
   }
   static void fImpl(UINT1* r,const T* v, size_t n) {
     for(size_t i=0; i< n; ++i)
       r[i]=!pcr::isMV(v[i]);
   }
 };

 template<typename T>
  struct SpatialArray: public IDiffUnSingleType<T> {
   SpatialArray() {
       this->f=(IDiffUn::F)fImpl;
   }
   // specialization for UINT1: memset(vL, *vR, n);
   static void fImpl(T* r,const T* v, size_t n) {
     PRECOND(!pcr::isMV(v[0]));
     for(size_t i=0; i< n; ++i)
       r[i]=v[0];
   }
 };

 struct PitArray: public IDiffUn {
   CRIndex cri() const { return CRI_1; }
   PitArray() {
       this->f=(F)fImpl;
   }
  static void fImpl(INT4* r,const UINT1* v, size_t n) {
    INT4   p=1;
    for(size_t i=0; i < n; ++i)
    switch(v[i]) {
       case MV_UINT1: r[i] = MV_INT4; break;
       case LDD_PIT : r[i] = p++; break;
       default      : r[i] = 0; break;
   }
  }
 };


}
