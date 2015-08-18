#ifndef INCLUDED_CALC_BRANCHEXPRIMPL
#define INCLUDED_CALC_BRANCHEXPRIMPL

#ifndef INCLUDED_CALC_BRANCHEXPR
# include "calc_branchexpr.h"
#define INCLUDED_CALC_BRANCHEXPR
#endif

#ifndef INCLUDED_CALC_FIELD
# include "calc_field.h"
#define INCLUDED_CALC_FIELD
#endif

namespace calc {

class Operator;
class FieldStack;

//! expression with operands of a function
/*  Describes a branch that holds a function or operator. Note that
 *  a function can have 0 arguments, thus holding no Leafs!
 * \todo
 *   not all execSomething needs an implOp arg, when identical to
 *     op, also op as arg conflicts with op()
 */
class BranchExprImpl : public BranchExpr {
 private:
  bool d_countStarted;

  void executeOperation(const Operator& implOp, FieldStack& stack);
  void executeVarArgOperation(const Operator& implOp, FieldStack& stack);

  void execSameUn(const Operator& op, FieldStack& stack);
  void execSameBin(const Operator& op, FieldStack& stack,
  bool leftSpatial, bool rightSpatial, bool argsAlreadyOnStack);
  void execGenNonSpatial(const Operator& op, FieldStack& stack);
  void execGenSpatial(const Operator& op, FieldStack& stack);
  void execDiffUn(const Operator& op, FieldStack& stack);
  void execIfThen(const Operator& op, FieldStack& stack);
  void execIfThenElse(const Operator& op, FieldStack& stack);
  void execDiffBin(const Operator& op, FieldStack& stack);
  void ExecMisc(const Operator& op, FieldStack& stack);
  void execGlob(const Operator& op, FieldStack& stack);
  void execExternal(const Operator& op, FieldStack& stack);

  FieldHandle conditionalBranch( bool skip, FieldExpr *branch, FieldStack& stack);
 public:
  // CREATORS
      BranchExprImpl(
    const Element&   pos,
    const Operator&  op,
          FieldExprArgs& fieldArgs);
  // MANIPULATORS
  void execute(class FieldStack& stack);
  //! for double assignment
  void executeDoubleAss(const Operator& implOp, FieldStack& stack, VS   resultVs[2]);

};

}

#endif
