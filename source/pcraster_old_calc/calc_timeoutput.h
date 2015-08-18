#ifndef INCLUDED_CALC_TIMEOUTPUT
#define INCLUDED_CALC_TIMEOUTPUT

#ifndef INCLUDED_CALC_STATEMENT
# include "calc_statement.h"
#define INCLUDED_CALC_STATEMENT
#endif

#ifndef INCLUDED_CALC_FIELDARGS
# include "calc_fieldargs.h"
#define INCLUDED_CALC_FIELDARGS
#endif

#ifndef INCLUDED_FIELDEXPRARGS
# include "calc_fieldexprargs.h"
#define INCLUDED_FIELDEXPRARGS
#endif

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
  bool d_buildTypesVisited;
 public:
  // CREATORS
  Timeoutput(
    const WriteInfo& w,
    const UsePar&  par,
          FieldExprArgs& args);
  virtual ~Timeoutput();
  // MANIPULATORS
  bool buildTypes();

  void prepareExecution();
  void run();

  // ACCESSORS
  void print(InfoScript& i)const;
};

}

#endif
