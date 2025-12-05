#ifndef INCLUDED_OLDCALC_TIMEINPUTEXPR
#define INCLUDED_OLDCALC_TIMEINPUTEXPR

#include "calc_mixedexpr.h"



namespace calc {

class UsePar;
class FieldStack;
class TssInputLeaf;

//! timeinput expression
/*! to support timeinput.... funcs
 */
class TimeinputExpr : public MixedExpr {
 private:
  // auto_ptr does not work!?
  TssInputLeaf *d_tss{nullptr};
  void cleanUp();
 public:
  TimeinputExpr(
    const Element& pos,
    const Operator& op,
          UsePar &tss,
          FieldExprArgs& keyArgs);
  ~TimeinputExpr() override;
  // MANIPULATORS
  void execute(FieldStack& s) override;
  //ACCESSORS
  void print(InfoScript& i)const override;
};
}

#endif
