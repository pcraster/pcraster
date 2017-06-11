#ifndef INCLUDED_CALC_FOPOINTARRAY
#define INCLUDED_CALC_FOPOINTARRAY

#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.
#ifndef INCLUDED_COM_CSFCELL
#include "com_csfcell.h"
#define INCLUDED_COM_CSFCELL
#endif

// Module headers.
#ifndef INCLUDED_CALC_IFOPOINTARRAY
#include "calc_ifopointarray.h"
#define INCLUDED_CALC_IFOPOINTARRAY
#endif

#ifndef INCLUDED_CALC_CR
#include "calc_cr.h"
#define INCLUDED_CALC_CR
#endif

#ifndef INCLUDED_CALC_DOMAINERROR
#include "calc_domainerror.h"
#define INCLUDED_CALC_DOMAINERROR
#endif

/*! \file
 *   templates for point operations implementations on array
 *   as function object with the op operator
 *
 * already checked that the inline domain checks
 * are optimized out when possible
 */

namespace calc {

#ifdef GCC
#define RESTRICT __restrict__
#else
#define RESTRICT
#endif

template<class O>
 //! process array of n elements
 struct SameUnArray: public ISameUn {
     typedef typename O::Type T;

     typedef T* Val;
     SameUnArray() {
       f=(F)fImpl;
     }
     static void fImpl(Val val, size_t n)
     {
       for(size_t i=0; i < n; ++i)
        if (!pcr::isMV(val[i])) // result already mv
          if (O::domainOk(val[i]))
            O::op(val[i]);
    }
 };

template<class O>
 //! process array of n elements
 struct DiffUnArray: public IDiffUn {
     typedef       typename O::ResultType * RESTRICT Result; // (out-only)
     typedef const typename O::InputType  * RESTRICT Input;

     CRIndex cri() const {
        return crIndex<typename O::InputType>();
     }
     DiffUnArray() {
       f=(F)fImpl;
     }
     virtual ~DiffUnArray() {
     }
     static void fImpl(Result r, Input val, size_t n)
     {
       for(size_t i=0; i < n; ++i) {
        pcr::setMV(r[i]);
        if (!pcr::isMV(val[i]))
          O::op(r[i],val[i]);
       }
    }
 };

template<class O>
 //! process array of n elements
 struct SameBinArray: public ISameBin {
     typedef typename O::Type T;

     typedef const T * RESTRICT Input; // Input
     typedef       T * RESTRICT Result; // Result (and input)

     CRIndex cri() const {
        return crIndex<T>();
     }

     SameBinArray()
     {
       ss=(SS)ssImpl;
       ns=(NS)nsImpl;
       sn=(SN)snImpl;
     }
     virtual ~SameBinArray()
     {
     }

     //! check r and combo of r and l
     inline static bool domainIll(const T& r, const T& l) {
       return O::rightDomainIll(r) || O::combDomainIll(r,l);
     }

     static void ssImpl(Result l, Input r, size_t n)
     {
       for(size_t i=0; i < n; ++i) {
        if (!((pcr::isMV(l[i])|pcr::isMV(r[i]))
              || domainIll(r[i],l[i])))
            O::op(l[i],l[i],r[i]);
          else
            pcr::setMV(l[i]);
      }
     }
     static void nsImpl(Input l, Result r, size_t n)
     {
        DEVELOP_PRECOND(!pcr::isMV(*l));
        T lV=*l;
        for(size_t i=0; i < n; ++i)
        if (!pcr::isMV(r[i])) // result already mv
        {
          if (domainIll(r[i],lV))
            pcr::setMV(r[i]);
          else
            O::op(r[i],lV,r[i]);
        }
     }

     static void snImpl(Result l, Input r, size_t n)
     {
        DEVELOP_PRECOND(!pcr::isMV(*r));
        T rV=*r;
        if (O::rightDomainIll(rV))
          throw DomainError();

        for(size_t i=0; i < n; ++i)
         if (!pcr::isMV(l[i])) // result already mv
         {
          if (domainIll(rV,l[i]))
            pcr::setMV(l[i]);
          else
            O::op(l[i],l[i],rV);
         }
     }
 };

template<typename T>
 struct CoverArray: public ISameBin {
     typedef const T* Input; // Input
     typedef       T* Result; // Result (and input)

     CRIndex cri() const {
        return crIndex<T>();
     }

     CoverArray()
     {
       ss=(SS)ssImpl;
       ns=(NS)nsImpl;
       sn=(SN)snImpl;
     }

     static void ssImpl(Result l, Input r, size_t n)
     {
       for(size_t i=0; i < n; ++i)
        if (pcr::isMV(l[i]))
          l[i]=r[i];
     }
     static void nsImpl(Input l, Result r, size_t n)
     {
       // the most left arg of cover in nonspatial
       // and thus non-spatial, result is always l
        DEVELOP_PRECOND(!pcr::isMV(*l));
        T lV=*l;
        for(size_t i=0; i < n; ++i)
          r[i]=lV;
     }

     static void snImpl(Result l, Input r, size_t n)
     {
        DEVELOP_PRECOND(!pcr::isMV(*r));
        T rV=*r;
        for(size_t i=0; i < n; ++i)
         if (pcr::isMV(l[i]))
           l[i]=rV;
     }
 };

template<typename T>
 struct IfThenArray: public IDiffBin {
     typedef       T    * Result; // Result
     typedef const UINT1* Cond;   // A1
     typedef const T    * True;   // A2

     //! used for 2nd arg, A2 that equal Result
     CRIndex cri() const {
        return crIndex<T>();
     }

     IfThenArray()
     {
       ss=(F)ssImpl;
       ns=(F)nsImpl;
       sn=(F)snImpl;
     }

     static void op(T& r, const UINT1& c, const T& v)
     {
       if (c==1)
         r=v;
       else
         pcr::setMV(r);
     }

     static void ssImpl(Result r, Cond cond, True trueV, size_t n)
     {
       for(size_t i=0; i < n; ++i)
         op(r[i],cond[i],trueV[i]);
     }
     static void nsImpl(Result r, Cond cond, True trueV, size_t n)
     {
        DEVELOP_PRECOND(!pcr::isMV(*cond));
        UINT1 c=*cond;
        for(size_t i=0; i < n; ++i)
         op(r[i],c,trueV[i]);
     }

     static void snImpl(Result r, Cond cond, True trueV, size_t n)
     {
        DEVELOP_PRECOND(!pcr::isMV(*trueV));
        T tV=*trueV;
        for(size_t i=0; i < n; ++i)
         op(r[i],cond[i],tV);
     }
 };



/*! partial implementation of ifthenelse, the case
 *  for a nonspatial condition is solved within IfThenElse
 */
template<typename T>
 struct IfThenElseArray: public IIfThenElse {
   //! Result, alway spatial here
   typedef       T    * Result;
   //! always spatial here
   typedef const UINT1* Cond;
   //!  type of both true and false branch
   typedef const T    * Branch;


   //! used for 2nd arg: true branch
   CRIndex cri() const {
      return crIndex<T>();
   }

   IfThenElseArray()
   {
     ss=(F)ssImpl;
     ns=(F)nsImpl;
     sn=(F)snImpl;
     nn=(F)nnImpl;
   }

   static void op(T& r, const UINT1& c, const T& trueV, const T& falseV)
   {
     switch(c) {
       case 0: r=falseV; break;
       case 1: r=trueV;  break;
       case MV_UINT1: pcr::setMV(r); break;
     }
   }

   static void ssImpl(Result r, Cond cond, Branch trueV, Branch falseV, size_t n)
   {
     for(size_t i=0; i < n; ++i)
       op(r[i],cond[i],trueV[i],falseV[i]);
   }
   static void nsImpl(Result r, Cond cond, Branch trueV, Branch falseV, size_t n)
   {
      DEVELOP_PRECOND(!pcr::isMV(*trueV));
      T t=*trueV;
      for(size_t i=0; i < n; ++i)
       op(r[i],cond[i],t,falseV[i]);
   }
   static void snImpl(Result r, Cond cond, Branch trueV, Branch falseV, size_t n)
   {
      DEVELOP_PRECOND(!pcr::isMV(*falseV));
      T f=*falseV;
      for(size_t i=0; i < n; ++i)
       op(r[i],cond[i],trueV[i],f);
   }
   static void nnImpl(Result r, Cond cond, Branch trueV, Branch falseV, size_t n)
   {
      DEVELOP_PRECOND(!pcr::isMV(*trueV));
      T t=*trueV;
      T f=*falseV;
      for(size_t i=0; i < n; ++i)
       op(r[i],cond[i],t,f);
   }
 };

template<class O>
 //! process array of n elements
 /*!
  * \todo
  *   not optimal for ne and eq -> sameBin fo's
  */
 struct DiffBinArray: public IDiffBin {
     typedef typename O::Type T;
     typedef UINT1*        R;
     //! A1 == A2 == E
     typedef const   T*    E;

     CRIndex cri() const {
        return crIndex<T>();
     }
     DiffBinArray() {

       ss=(F)ssImpl;
       ns=(F)nsImpl;
       sn=(F)snImpl;
     }
     virtual ~DiffBinArray() {
     }
     static void ssImpl(R res, E l, E r,size_t n)
     {
       for(size_t i=0; i < n; ++i) {
         pcr::setMV(res[i]);
         if (!(pcr::isMV(l[i])|pcr::isMV(r[i])))
            O::op(res[i],l[i],r[i]);
       }
     }
     static void nsImpl(R res, E l, E r, size_t n)
     {
        DEVELOP_PRECOND(!pcr::isMV(*l));
        T lV=*l;
        for(size_t i=0; i < n; ++i) {
         pcr::setMV(res[i]);
         if (!pcr::isMV(r[i]))
           O::op(res[i],lV,r[i]);
        }
     }
     static void snImpl(R res, E l, E r, size_t n)
     {
        DEVELOP_PRECOND(!pcr::isMV(*r));
        T rV=*r;
        for(size_t i=0; i < n; ++i) {
         pcr::setMV(res[i]);
         if (!pcr::isMV(l[i]))
           O::op(res[i],l[i],rV);
        }
     }
 };

 /*! agregate ->return nonspatial
 *  kan als met state in point-code, is
 *  "cache-friendly" de state is relatief klein, net zoals pit functie
 */
 template<class O>
 struct AggregateArray: public IDiffUn {
   typedef       typename O::AggregateType* R;
   typedef const typename O::Input*         I;

   CRIndex cri() const {
       return crIndex<typename O::Input>();
   }

   AggregateArray() {
       f=(F)fImpl;
   }
   virtual ~AggregateArray() {
   }

   static void fImpl(R r,I v, size_t n) {
     *r=O::init();
     for(size_t i=0; i< n; ++i)
       if (!pcr::isMV(v[i]))
        O::op(*r,v[i]);
   }
 };
}

#endif
