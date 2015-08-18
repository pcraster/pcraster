#ifndef INCLUDED_CALC_DOUBLEASS
#define INCLUDED_CALC_DOUBLEASS

#ifndef INCLUDED_CALC_STATEMENT
# include "calc_statement.h"
#define INCLUDED_CALC_STATEMENT
#endif

#ifndef INCLUDED_CALC_FIELDEXPRARGS
# include "calc_fieldexprargs.h"
#define INCLUDED_CALC_FIELDEXPRARGS
#endif

#ifndef INCLUDED_CALC_OPERATOR
#include "calc_operator.h"
#define INCLUDED_CALC_OPERATOR
#endif

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
  FieldLeft *d_left[2];
  BranchExprImpl *d_right;
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
  ~DoubleAssignment();

  bool buildTypes() ;
  void prepareExecution();
  void run();

  // ACCESSORS
  void print(InfoScript& i)const;
};

MAJOR_CODE dassImplementor(MAJOR_CODE op);
int stackPosition(MAJOR_CODE op);
MAJOR_CODE otherDouble(MAJOR_CODE op);

void wrongDoubleAssignment(const Element& left0,const Operator& func0,const Operator& func1); // ALWAYS USER ERROR
}

#endif
