#ifndef INCLUDED_CALC_FOPOINT
#define INCLUDED_CALC_FOPOINT

#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.
#ifndef INCLUDED_PCRTYPES
#include "pcrtypes.h"
#define INCLUDED_PCRTYPES
#endif

// Module headers.


/*! \file
 *   templates for point operations implementations on array
 *   as function object with the op operator
 */


namespace calc {

 template<typename Result,
          typename Input>
 struct PointOpTypes {
   typedef Result ResultType;
   typedef Input  InputType;
 };

 template<size_t  nr>
  struct NrInputs {
    enum { N = nr };
 };

 template<typename T>
 struct SameBinPoint : public PointOpTypes<T,T>, public NrInputs<2> {
   typedef T Type;

   //! is right domain illegal? for example, div by zero
   inline static bool rightDomainIll(const T&) {
     return false;
   }
   //! combination of r and l domain illegal only used for pow (**)
   inline static bool combDomainIll(const T&, const T&) {
     return false;
   }
 };

 namespace detail {
  template<typename T>
   struct NoDomain {
    //! is domain illegal?
    inline static bool onlyDomainIll(const T&) {
      return false;
    }
   };
 }


 template<typename T,
          class    Derived=detail::NoDomain<T>
 >
 struct SameUnPoint : public PointOpTypes<T,T>, public NrInputs<1> {
   typedef T Type;
   //! is domain ok? if not v is set to MV
   inline static bool domainOk(T& v) {
     if (Derived::onlyDomainIll(v)) {
       pcr::setMV(v);
       return false;
     }
     return true;
   }
 };

 template<typename Result,
          typename Input>
 struct DiffUnPoint:
   public PointOpTypes<Result,Input>,
   public NrInputs<1>
   {};

 template<typename T>
 struct DiffBinPoint : public PointOpTypes<UINT1,T>, public NrInputs<2>
 {
   typedef T Type;
 };


//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace calc

#endif
