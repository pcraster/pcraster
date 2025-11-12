#ifndef INCLUDED_CALC_REPEATUNTIL
#define INCLUDED_CALC_REPEATUNTIL

#include "calc_innerstatementblock.h"


namespace calc {
class FieldExpr;
//! repeat until block
/*! The structure of a repeat until block is
 *  <PRE>
 *    repeat {
 *     "statementblock"
 *    } until "fieldExpr" ;
 *  </PRE>
 *  Where fielExpr must be of boolean type
 */
class RepeatUntil : public InnerStatementBlock {
private:
  FieldExpr *d_condition{nullptr};

  bool executeCondition();
 public:
  //! Idlist classes are user_symbols of type IndexContainer
  RepeatUntil(
    const Element& pos,
    class StatementBlock *parentBlock);

  ~RepeatUntil() override;

 // MANIPULATORS
  bool buildTypes() override;
//  void prepareExecution();
  void executeBlock() override;
  void addCondition(FieldExpr* condition);

  void print(InfoScript& i)const override;
};

}

#endif
