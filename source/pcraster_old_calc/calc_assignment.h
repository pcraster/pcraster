#ifndef INCLUDED_CALC_ASSIGNMENT
#define INCLUDED_CALC_ASSIGNMENT

#ifndef INCLUDED_CALC_STATEMENT
# include "calc_statement.h"
#define INCLUDED_CALC_STATEMENT
#endif


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
  FieldLeft  *d_left;
  FieldExpr  *d_right;

  void cleanUp();

 public:
  // CREATORS
  Assignment( StatementBlock* b,
       const WriteInfo&      w,
       const UsePar&         par,
             FieldExpr*      right);

  virtual ~Assignment();

  bool buildTypes();

  void prepareExecution();
  void run() ;

  // ACCESSORS
  void print(InfoScript& i)const;
};

}

#endif
