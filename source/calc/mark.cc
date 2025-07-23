#include "stddefx.h"

#include "calc.h"  // for it's own interface

#include "geo_celllocvisitor.h"
#include "fieldapi_interface.h"

#include <algorithm>
#include <cmath>
#include <vector>

namespace calc {

class MarkPoint {
private:
  geo::CellLoc d_c;
  float d_order;
  float d_amount;
public:
      MarkPoint(const geo::CellLoc& c, double order, double amount):
                 d_c(c),
                 d_order(POSSIBLE_DATA_LOSS(float,order)),
                 d_amount(POSSIBLE_DATA_LOSS(float,amount))
                 {}
      double amount() const {
         return d_amount;
      }
      double order() const {
         return d_order;
      }
      const geo::CellLoc& cellLoc() {
         return d_c;
      }
};

class CmpSortKey {
 public:
  bool operator()(const MarkPoint& e1, const MarkPoint& e2) {
   return e1.order() < e2.order();
  }
};

class MarkCondition {
 protected:
  double d_sum, d_treshold;
 public:
  MarkCondition(double treshold):
    d_sum(0),d_treshold(treshold)
    {}
  virtual bool mark(double value)=0;
};
class MarkLe : public MarkCondition {
  public:
    MarkLe(double treshold):
     MarkCondition(treshold)
    {}
   bool mark(double value) override {
     d_sum+=value;
     return d_sum <= d_treshold;
   }
};
class MarkGe : public MarkCondition {
  public:
    MarkGe(double treshold):
     MarkCondition(treshold)
    {}
   bool mark(double value) override {
     bool t=  (d_sum < d_treshold);
     d_sum+=value;
     return t;
   }
};

static int MarkWhileSum(
     MAP_UINT1 *m_resultMap,
     const MAP_REAL8 *m_order,
     const MAP_REAL8 *m_amount,
     calc::MarkCondition *markCondition)
{
  ReadWriteUint1_ref(result,m_resultMap);
  ReadOnlyReal8_ref(order,m_order);
  ReadOnlyReal8_ref(amount,m_amount);

  std::vector<MarkPoint> points;

   for(geo::CellLocVisitor c(result); c.valid(); ++c) {
     REAL8 orderVal = NAN;
     REAL8 amountVal = NAN;
     if (order.get(orderVal, *c) && amount.get(amountVal, *c))
      points.push_back(MarkPoint(*c,orderVal,amountVal));
     else
      result.putMV(*c);
  }

  std::sort(points.begin(),points.end(),CmpSortKey());

  UINT1  mark=1;
  for(auto & point : points) {
    geo::CellLoc c(point.cellLoc());
    if (mark) {
     mark = markCondition->mark(point.amount());
    }
    // std::cout << c << " " << (int)mark << "\n";
    result.put(mark,c);
  }

 return 0;
}

}

extern "C" int MarkWhileSumLe(
     MAP_UINT1 *m_resultMap,
     const MAP_REAL8 *m_order,
     const MAP_REAL8 *m_amount,
     const MAP_REAL8 *m_treshold)
{
  ReadOnlyReal8_ref(tresholdMap,m_treshold);
  PRECOND(!tresholdMap.spatial());
  calc::MarkLe t(tresholdMap.value(0,0));
  return calc::MarkWhileSum( m_resultMap, m_order, m_amount, &t);
}

extern "C" int MarkUntilSumGe(
     MAP_UINT1 *m_resultMap,
     const MAP_REAL8 *m_order,
     const MAP_REAL8 *m_amount,
     const MAP_REAL8 *m_treshold)
{
  ReadOnlyReal8_ref(tresholdMap,m_treshold);
  PRECOND(!tresholdMap.spatial());
  calc::MarkGe t(tresholdMap.value(0,0));
  return calc::MarkWhileSum( m_resultMap, m_order, m_amount, &t);
}
