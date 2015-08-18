#ifndef INCLUDED_CALC_SYMBOLTABLE
#define INCLUDED_CALC_SYMBOLTABLE

#ifndef INCLUDED_MAP
#include <map>
#define INCLUDED_MAP
#endif

#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif

#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
#endif

#ifndef INCLUDED_CALC_VS
#include "calc_vs.h"
#define INCLUDED_CALC_VS
#endif

namespace pcrxml {
  class Data;
}

namespace calc {

class ArcViewExtCheckData;
class Symbol;
class UserSymbol;
class SubParameter;
class ParsPar;
class InfoScript;
class StatementBlock;

class SymbolTable {
  typedef std::pair<std::string, UserSymbol *>ParameterTableItem;
  typedef std::map <std::string, UserSymbol *>ParameterTable;
  typedef ParameterTable::const_iterator ConstIter;
  typedef ParameterTable::iterator Iter;

  ParameterTable  d_table;

  //! parent statement block, 0 if no parent (= whole script)
  StatementBlock *d_parentBlock;

  //! holds next symbolSequenceNr to assign to a UserSymbol
  int d_symbolSequenceNr;

  //! Find an existing parameter in own table, return 0 if not found
  UserSymbol *find(const std::string& name) const;

  SubParameter *findParameter(
    const ParsPar&      par,
          VS                 expectedVs,
          bool               mustExist) const;


 public:
  SymbolTable(StatementBlock *b);
   ~SymbolTable();

  void add(UserSymbol *sym);
  void goInScope();
  void finalCheck();

  // ACCESSORS

  //! find and verifies correct use of parameter
  /*! returns 0 if the parameter does not yet exit in the symbol
   * table. If it does not exist, it does check if par is available
   * on the filesystem in the case of non-array. Array's that does
   * not exist, or are used with the wrong descriptor will throw an
   * exception .
   */
  SubParameter *findRightParameter(
      const ParsPar& par,
    VS expectedVs ) const;

  //! find and verifies correct use of parameter
  /*! returns 0 if the parameter does not yet exists in the symbol table
   * Parameters that does
   * exist, but are used with the wrong descriptor will throw an
   * exception .
   */
  SubParameter *findLeftParameter(
    const ParsPar& par,
    VS expectedVs) const;

  //! recursive search through all blocks, returning 0 if not found
   UserSymbol *find(
    const Symbol *sym,
    VS typeExpected,
    bool mustExist) const;


  //! print info
  void print(InfoScript& i)const;

  void createXmlData(std::vector<pcrxml::Data *>& addHere) const;

  void setArcViewExtCheckData(std::vector<ArcViewExtCheckData>& r) const;

};

}

#endif
