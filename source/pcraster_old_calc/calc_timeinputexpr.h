#ifndef INCLUDED_CALC_TIMEINPUTEXPR
#define INCLUDED_CALC_TIMEINPUTEXPR

#ifndef INCLUDED_CALC_MIXEDEXPR
# include "calc_mixedexpr.h"
#define INCLUDED_CALC_MIXEDEXPR
#endif

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
  TssInputLeaf *d_tss;
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
