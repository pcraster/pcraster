#ifndef INCLUDED_STDOUTSTATEMENT
#define INCLUDED_STDOUTSTATEMENT

# include "calc_statement.h"

# include <memory>



namespace calc {

class FieldExpr;

//! an expression that print to stdout
class StdoutStatement : public Statement {
  std::unique_ptr<FieldExpr> d_expr;
 public:
  // CREATORS
  StdoutStatement(
    FieldExpr *right);

  bool buildTypes() override;
  void prepareExecution() override;
  void run() override;

  // ACCESSORS
  void print(InfoScript& i)const override;
};

}

#endif
