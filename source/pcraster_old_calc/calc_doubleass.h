#ifndef INCLUDED_CALC_DOUBLEASS
#define INCLUDED_CALC_DOUBLEASS

#include "calc_statement.h"
#include "calc_fieldexprargs.h"
#include "calc_operator.h"


namespace calc {

class FieldLeft;
class BranchExprImpl;
class WriteInfo;
class UsePar;
class StatementBlock;




class DoubleAssignment : public Statement {
 private:
  bool d_swapped;
  const Operator &d_op0, &d_op1;
  FieldLeft *d_left[2]{};
  BranchExprImpl *d_right{nullptr};
  void cleanUp();
 public:
  DoubleAssignment(
    StatementBlock *b,
    const Element& pos,
    const WriteInfo& w,
    const UsePar& p0,
    const UsePar& p1,
    const Element& posFunc,
    const Operator& f0,
    const Operator& f1,
          FieldExprArgs& args);
  ~DoubleAssignment() override;

  bool buildTypes() override ;
  void prepareExecution() override;
  void run() override;

  // ACCESSORS
  void print(InfoScript& i)const override;
};

MAJOR_CODE dassImplementor(MAJOR_CODE op);
int stackPosition(MAJOR_CODE op);
MAJOR_CODE otherDouble(MAJOR_CODE op);

void wrongDoubleAssignment(const Element& left0,const Operator& func0,const Operator& func1); // ALWAYS USER ERROR
}

#endif
