#ifndef INCLUDED_CALC_LOOKUPEXPR
#define INCLUDED_CALC_LOOKUPEXPR

#ifndef INCLUDED_CALC_MIXEDEXPR
# include "calc_mixedexpr.h"
#define INCLUDED_CALC_MIXEDEXPR
#endif

#ifndef INCLUDED_FIELDEXPRARGS
# include "calc_fieldexprargs.h"
#define INCLUDED_FIELDEXPRARGS
#endif

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
  virtual ~LookupExpr();
  // MANIPULATORS
  void execute(FieldStack& s);
  //ACCESSORS
  void print(InfoScript& i)const;
};

}

#endif
