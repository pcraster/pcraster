#ifndef INCLUDED_CALC_TIMEOUTPUT
#define INCLUDED_CALC_TIMEOUTPUT

# include "calc_statement.h"
# include "calc_fieldargs.h"
# include "calc_fieldexprargs.h"



namespace calc {

class WriteInfo;
class StatementBlock;
class UsePar;
class TssOutputParameter;
class IndexSelected;

/*!
 * \todo
 *   try recognizing and warn on constucts as:
 *    timeoutput(1,maptotal(.....)/(maparea(Rain)/(DX*DX)));
 */
class Timeoutput : public FieldArgs, public Statement {
  TssOutputParameter *d_par;
  IndexSelected* d_index;
  bool d_buildTypesVisited{false};
 public:
  // CREATORS
  Timeoutput(
    const WriteInfo& w,
    const UsePar&  par,
          FieldExprArgs& args);
  ~Timeoutput() override;
  // MANIPULATORS
  bool buildTypes() override;

  void prepareExecution() override;
  void run() override;

  // ACCESSORS
  void print(InfoScript& i)const override;
};

}

#endif
