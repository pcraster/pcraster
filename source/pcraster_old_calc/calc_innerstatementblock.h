#ifndef INCLUDED_OLDCALC_INNERSTATEMENTBLOCK
#define INCLUDED_OLDCALC_INNERSTATEMENTBLOCK

#include "calc_statementblock.h"


namespace calc {

class InnerStatementBlock : public StatementBlock {
 public:
  // CREATORS
  InnerStatementBlock(const Element& p,
    StatementBlock *parentBlock);

  // DESTRUCTORS
  ~InnerStatementBlock() override;

  void executeBlock() override;

  // ACCESSORS
  void print(InfoScript& i) const override;
};

}

#endif
