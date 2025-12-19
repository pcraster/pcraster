#ifndef INCLUDED_CALC_POINTCODEDLLHEADER
#define INCLUDED_CALC_POINTCODEDLLHEADER

/*!
  \file
  This file contains all header stuff needed in a dll to compile
  for point code generation
*/

#include "stddefx.h"
#include "com_csfcell.h"
#include "calc_fopointimpl.h"
#include "calc_cellunion.h"


namespace calc {

// unit test of these inlines in calc_pointcodeblockdlltest.cc

//! only used in DLL
template<typename A /* UINT1/INT4/REAL4 */>
struct _ifthenelseClass {
  static inline void op(A &result, const UINT1& cond, const A& arg1,const A& arg2) {
    if(pcr::isMV(cond)) { // condition
      pcr::setMV(result);
    } else {
      if(cond != 0u) {
        if(pcr::isMV(arg1)) {
          pcr::setMV(result);
        } else {
          result=arg1;
        }
      } else {
        if(pcr::isMV(arg2)) {
          pcr::setMV(result);
        } else {
          result=arg2;
        }
      }
    }
  }
 };


template
<class A>
 inline void _ifthenelse(A &result, const UINT1& cond, const A& arg1,const A& arg2) {
  _ifthenelseClass<A>::op(result, cond, arg1,arg2);
 }

template void _ifthenelse(UINT1& result, const UINT1& cond, const UINT1& arg1, const UINT1& arg2);
template void _ifthenelse(INT4& result, const UINT1& cond, const INT4& arg1, const INT4& arg2);
template void _ifthenelse(REAL4& result, const UINT1& cond, const REAL4& arg1, const REAL4& arg2);

template
 <class T>
 inline bool _rdi(const typename T::Type& v) {
   return T::rightDomainIll(v);
 }

template
 <class T>
 inline bool _odi(const typename T::Type& v) {
   return T::onlyDomainIll(v);
 }

template
 <class T>
 inline bool _cdi(const typename T::Type& v1, const typename T::Type& v2) {
   return T::combDomainIll(v1,v2);
 }


// T == DiffUnPoint/SameUnPoint
template
 <class T>
 inline typename T::ResultType _f(const typename T::InputType& v)
 {
   return T::f(v);
 }

// T == SameBinPoint/DiffBinPoint
template
 <class T>
 inline typename T::ResultType _f(const typename T::InputType& v1,
                                  const typename T::InputType& v2)
 {
   return T::f(v1,v2);
 }

}

#endif
