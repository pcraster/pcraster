#ifndef INCLUDED_CALC_LOOKUPEXPR
#define INCLUDED_CALC_LOOKUPEXPR

# include "calc_mixedexpr.h"
# include "calc_fieldexprargs.h"



namespace calc {

class UsePar;
class StatementBlock;
class FieldStack;
class LookupTableLeaf;

//! lookup expression
/* designed for lookup.... funcs 
 */
class LookupExpr : public MixedExpr {
 private:
  LookupTableLeaf* init(const UsePar &tab);
  void cleanUp();
  LookupTableLeaf* d_tab;
 public:
  // CREATORS
  LookupExpr(
    const Element& pos,
    const Operator& op,
    const UsePar &tab,
          FieldExprArgs& keyArgs);
  ~LookupExpr() override;
  // MANIPULATORS
  void execute(FieldStack& s) override;
  //ACCESSORS
  void print(InfoScript& i)const override;
};

}

#endif
