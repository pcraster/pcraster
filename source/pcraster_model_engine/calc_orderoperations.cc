#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_ORDEROPERATIONS
#include "calc_orderoperations.h"
#define INCLUDED_CALC_ORDEROPERATIONS
#endif

// Library headers.
#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
#endif
#ifndef INCLUDED_CASSERT
#include <cassert>
#define INCLUDED_CASSERT
#endif

// PCRaster library headers.
#ifndef INCLUDED_COM_MVGENERIC
#include "com_mvgeneric.h"
#define INCLUDED_COM_MVGENERIC
#endif

namespace calc {
namespace detail {

  class  OrderSortFO {
   IVSpatial<double> const* d_val;
   IVSpatial<double> const& val() const {
     return *d_val;
   }
  public:
   OrderSortFO(IVSpatial<double> const& val):
       d_val(&val) {};
   //! sort criteria on val
   bool operator()(size_t e1, size_t e2) const {
      assert(!pcr::isMV(val()[e1]));
      assert(!pcr::isMV(val()[e2]));
      return val()[e1] < val()[e2];
    }
  };

  class  AreaOrderSortFO {
   IVSpatial<double> const* d_exprVal;
   IVSpatial<INT4>   const* d_classVal;
   IVSpatial<double> const& exprVal() const {
     return *d_exprVal;
   }
   IVSpatial<INT4> const& classVal() const {
     return *d_classVal;
   }
  public:
   AreaOrderSortFO(
   IVSpatial<double> const& exprVal,
   IVSpatial<INT4>   const& classVal):
     d_exprVal(&exprVal),d_classVal(&classVal)
   {};
   //! sort criteria on val
   bool operator()(size_t e1, size_t e2) const {
      assert(!pcr::isMV( exprVal()[e1]));
      assert(!pcr::isMV( exprVal()[e2]));
      assert(!pcr::isMV(classVal()[e1]));
      assert(!pcr::isMV(classVal()[e2]));
      if (classVal()[e1]==classVal()[e2])
         return exprVal()[e1] < exprVal()[e2];
      else
         return classVal()[e1] < classVal()[e2];
    }
  };
 } // namespace detail
} // namespace calc

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



void calc::orderOperation(
                    REAL4 *res,
                    IVSpatial<double> const&  val,
                    size_t len)
{
  std::vector<size_t> order;
  // maximum size to can be used:
  order.reserve(len);

  // add non-MV indeces to order
  for(size_t i=0; i < len; ++i)
    if (val.isMV(i))
      pcr::setMV(res[i]);
    else
      order.push_back(i);

  // order on val, index in order is the order nr
  detail::OrderSortFO sort(val);
  std::sort(order.begin(),order.end(),sort);

  for(size_t i=0; i < order.size(); ++i) {
    assert(order[i] < len);
    // index in order (i) is the order nr
    res[order[i]]=(REAL4)i+1;
  }
}

void calc::areaOrderOperation(
              REAL4 *res,
              IVSpatial<double> const&  expr,
              IVSpatial<INT4>   const&  areaClass,
              size_t len)
{
  std::vector<size_t> order;
  // maximum size to can be used:
  order.reserve(len);

  // add non-MV indeces to order
  for(size_t i=0; i < len; ++i)
    if (expr.isMV(i) || areaClass.isMV(i))
      pcr::setMV(res[i]);
    else
      order.push_back(i);

  // order on val, index in order is the order nr
  detail::AreaOrderSortFO sort(expr,areaClass);
  std::sort(order.begin(),order.end(),sort);

  INT4   prevClass=MV_INT4;
  REAL4  orderNr=1;
  for(size_t i=0; i < order.size(); ++i) {
    assert(order[i] < len);
    if (areaClass[order[i]] != prevClass) {
      orderNr=1;
    }
    res[order[i]]=orderNr;
    orderNr += 1;
    prevClass=areaClass[order[i]];
  }
}
