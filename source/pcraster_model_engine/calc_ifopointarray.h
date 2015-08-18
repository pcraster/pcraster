#ifndef INCLUDED_CALC_IFOPOINTARRAY
#define INCLUDED_CALC_IFOPOINTARRAY

#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.
#ifndef INCLUDED_CSFTYPES
#include "csftypes.h"  // UINT1
#define INCLUDED_CSFTYPES
#endif

// Module headers.
#ifndef INCLUDED_CALC_CR
#include "calc_cr.h"
#define INCLUDED_CALC_CR
#endif


/*! \file
 *   templates for point operations interfaces on array
 *   as function object with the op operator
 */

namespace calc {

  //! interface specification, class must have a cri() function
  struct ISelectOnCRIndex {
    virtual CRIndex cri() const =0;
    virtual ~ISelectOnCRIndex() {};
  };

  struct ISameUn {
   typedef       void* Val;

   typedef void (*F)(Val l, size_t n);
   F f;
  };

  struct IDiffUn : public ISelectOnCRIndex {
   virtual ~IDiffUn() {};
   typedef const void* Input;
   typedef       void* Result; // (out-only)

   typedef void (*F)(Result r, Input v, size_t n);
   F f;
  };

  struct ISameBin : public ISelectOnCRIndex {
   virtual ~ISameBin() {};
   typedef const void* Input;  // Input
   typedef       void* Result; // Result (and input)

   typedef void (*SS)(Result l, Input  r, size_t n);
   typedef void (*NS)(Input  l, Result r, size_t n);
   typedef void (*SN)(Result l, Input  r, size_t n);

   SS ss;
   NS ns;
   /*!
      \throws DomainError()
    */
   SN sn;
  };

  /*! two input arguments with (possible) different types
   *  and a new created result type R
   */
  struct IDiffBin : public ISelectOnCRIndex {
   virtual ~IDiffBin() {};
   typedef UINT1*        R;
   typedef const void*   A1;
   typedef const void*   A2;

   typedef void (*F)(R r, A1 a1, A2 a2,size_t n);

   F ss;
   F ns;
   F sn;
  };

 struct IIfThenElse : public ISelectOnCRIndex {
   virtual ~IIfThenElse() {};
   typedef void (*F)(void* r, const UINT1* c, const void *t, const void *f,size_t n);

   /*! dimension of true and false branch make different entries
    *  the result is always spatial
    */
   F ss;
   F ns;
   F sn;
   F nn;
 };
}

#endif
