#ifndef INCLUDED_CALC_STATEMENTBLOCK
#define INCLUDED_CALC_STATEMENTBLOCK

#ifndef INCLUDED_CALC_STATEMENT
#include "calc_statement.h"
#define INCLUDED_CALC_STATEMENT
#endif

#ifndef INCLUDED_LIST
#include <list>
#define INCLUDED_LIST
#endif

#ifndef INCLUDED_MAP
#include <map>
#define INCLUDED_MAP
#endif

#ifndef INCLUDED_CALC_VS
#include "calc_vs.h"
#define INCLUDED_CALC_VS
#endif

namespace calc {

class ParsPar;
class SubParameter;
class UserSymbol;
class FieldParameter;


class StatementBlock : public Statement {
 private:
  //! parameters whose values can be removed after executing this block
  typedef std::list<FieldParameter *> FieldParList;
  FieldParList d_valueDelete;

  //! to which block this statement belongs to, 0 for script
  StatementBlock* d_parentBlock;

 protected:
  // CREATORS
  StatementBlock(const Element& p,
    StatementBlock *parentBlock);

  // ACCESSORS

  typedef std::list<Statement *> StatList;
  StatList   d_stats;

 public:
  // DESTRUCTORS
  virtual ~StatementBlock();

  //! add statement add end of block
  void addStatement(Statement *s);


  virtual bool buildTypes();


  virtual void prepareExecution();
  virtual void run();
  void executeStatements();
  virtual void executeBlock()=0;

  //! Script implements, others redirect
  virtual void addSymbol(class UserSymbol *sym);

  //! return parent block, 0 if no parent 
  StatementBlock* parentBlock();

  //! return parent block, 0 if no parent 
  const StatementBlock* parentBlock()const;

  void deleteAtExit(FieldParameter *par);

  // ACCESSORS
  void printBlock(InfoScript& i)const;


  //! Script implements, others redirect
   virtual UserSymbol *findSymbol(
    const class Symbol *sym,
    VS typeExpected,
    bool mustExist) const;

  virtual bool isForEachBlock() const;
  virtual bool inDynamic() const;
};

bool operator==(
  const StatementBlock &b1,
  const StatementBlock &b2);

}

#endif
