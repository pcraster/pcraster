#ifndef INCLUDED_CALC_IFOPOINTARRAY
#define INCLUDED_CALC_IFOPOINTARRAY

#include "stddefx.h"
#include "csftypes.h"  // UINT1
#include "calc_cr.h"


/*! \file
 *   templates for point operations interfaces on array
 *   as function object with the op operator
 */

namespace calc {

  //! interface specification, class must have a cri() function
  struct ISelectOnCRIndex {
    virtual CRIndex cri() const =0;
    virtual ~ISelectOnCRIndex() {}
  };

  struct ISameUn {
   using Val = void *;

   using F = void (*)(Val, size_t);
   F f;
  };

  struct IDiffUn : public ISelectOnCRIndex {
   ~IDiffUn() override {}
   using Input = const void *;
   using Result = void *; // (out-only)

   using F = void (*)(Result, Input, size_t);
   F f{};
  };

  struct ISameBin : public ISelectOnCRIndex {
   ~ISameBin() override {}
   using Input = const void *;  // Input
   using Result = void *; // Result (and input)

   using SS = void (*)(Result, Input, size_t);
   using NS = void (*)(Input, Result, size_t);
   using SN = void (*)(Result, Input, size_t);

   SS ss{};
   NS ns{};
   /*!
      \throws DomainError()
    */
   SN sn{};
  };

  /*! two input arguments with (possible) different types
   *  and a new created result type R
   */
  struct IDiffBin : public ISelectOnCRIndex {
   ~IDiffBin() override {}
   using R = UINT1 *;
   using A1 = const void *;
   using A2 = const void *;

   using F = void (*)(R, A1, A2, size_t);

   F ss{};
   F ns{};
   F sn{};
  };

 struct IIfThenElse : public ISelectOnCRIndex {
   ~IIfThenElse() override {}
   using F = void (*)(void *, const UINT1 *, const void *, const void *, size_t);

   /*! dimension of true and false branch make different entries
    *  the result is always spatial
    */
   F ss{};
   F ns{};
   F sn{};
   F nn{};
 };
}

#endif
