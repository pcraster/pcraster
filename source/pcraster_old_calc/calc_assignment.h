#ifndef INCLUDED_OLDCALC_ASSIGNMENT
#define INCLUDED_OLDCALC_ASSIGNMENT

#include "calc_statement.h"


namespace calc {

class StatementBlock;
class FieldLeft;
class FieldExpr;
class WriteInfo;
class UsePar;

/*!
 * \todo
 *   rename Assignment to FieldAssignment
 */
class Assignment : public Statement {
  FieldLeft  *d_left{nullptr};
  FieldExpr  *d_right;

  void cleanUp();

 public:
  // CREATORS
  Assignment( StatementBlock* b,
       const WriteInfo&      w,
       const UsePar&         par,
             FieldExpr*      right);

  ~Assignment() override;

  bool buildTypes() override;

  void prepareExecution() override;
  void run() override ;

  // ACCESSORS
  void print(InfoScript& i)const override;
};

}

#endif
