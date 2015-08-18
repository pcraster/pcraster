#ifndef INCLUDED_CALC_INNERSTATEMENTBLOCK
#define INCLUDED_CALC_INNERSTATEMENTBLOCK

#ifndef INCLUDED_CALC_STATEMENTBLOCK
#include "calc_statementblock.h"
#define INCLUDED_CALC_STATEMENTBLOCK
#endif

namespace calc {

class InnerStatementBlock : public StatementBlock {
 public:
  // CREATORS
  InnerStatementBlock(const Element& p,
    StatementBlock *parentBlock);

  // DESTRUCTORS
  virtual ~InnerStatementBlock();

  virtual void executeBlock();

  // ACCESSORS
  virtual void print(InfoScript& i) const;
};

}

#endif
