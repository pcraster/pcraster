#ifndef INCLUDED_CALC_REPEATUNTIL
#define INCLUDED_CALC_REPEATUNTIL

#ifndef INCLUDED_CALC_INNERSTATEMENTBLOCK
# include "calc_innerstatementblock.h"
#define INCLUDED_CALC_INNERSTATEMENTBLOCK
#endif

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
  FieldExpr *d_condition;

  bool executeCondition();
 public:
  //! Idlist classes are user_symbols of type IndexContainer
  RepeatUntil(
    const Element& pos,
    class StatementBlock *parentBlock);

  virtual ~RepeatUntil();

 // MANIPULATORS
  bool buildTypes();
//  void prepareExecution();
  void executeBlock();
  void addCondition(FieldExpr* condition);

  void print(InfoScript& i)const;
};

}

#endif
