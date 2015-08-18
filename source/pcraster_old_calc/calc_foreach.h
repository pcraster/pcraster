#ifndef INCLUDED_CALC_FOREACH
#define INCLUDED_CALC_FOREACH

#ifndef INCLUDED_CALC_INNERSTATEMENTBLOCK
# include "calc_innerstatementblock.h"
#define INCLUDED_CALC_INNERSTATEMENTBLOCK
#endif

#ifndef INCLUDED_CALC_IDLIST
# include "calc_idlist.h"
#define INCLUDED_CALC_IDLIST
#endif

#ifndef INCLUDED_CALC_SYMBOLTABLE
# include "calc_symboltable.h"
#define INCLUDED_CALC_SYMBOLTABLE
#endif

#ifndef INCLUDED_SET
# include <set>
#define INCLUDED_SET
#endif

namespace calc {

class IndexParameter;

//! for each control
/*! The structure of a foreach is
 *  <BR>
 *   <B>foreach</B> <I>h</I> <B>in</B> Set (<B>except</B> Set)? 
 *   (<B>ascendingby|descendingby</B> Set)?  
 * Note<UL>
 *  <LI>Set can be an enclosing iterator, for example
 *  <PRE>
 *    foreach h in S {
 *     foreach i in S except h {
 *  </PRE>
 *  <LI>Since we can switch of elements, it is possible that the block
 *      never executes
 *  <LI>orderby must be defined better
 *  </UL>
 */
class ForEach : public InnerStatementBlock {
public:
  typedef std::set<const class IndexParameter *>Set;
private:
  void indexSet(Set& set,const IdList& idList);

  //! parameter here
  class UserSymbol *findSymbol(const class Symbol* sym, 
    VS typesExpected, bool mustExist) const; 

  //! add a parameter local to this block
  /*! currently only used for the index symbol
   */
  void addLocal(UserSymbol *par); 


  //! local symbol table
  SymbolTable  d_symTab;

  //! the symbol iterating (e.g. h), during parsing
  const Symbol d_iterSymbol;

  //! the array iterated over
  const class ArrayDefinition *d_loopedArray;

  //! the iterator
  class IndexParameterVariable *d_iter;

  //! the in clause
  Set d_in;

  //! the exclusive clause
  Set d_excl;

  //!  the order (ascendingby|descendingby) clause
  Set d_order;

  //!  ascending or descending of d_order
  bool  d_orderIsAscending;

  //! index during execution
  std::vector<const class IndexParameterConstant *>::const_iterator d_current;

 public:
  //! Idlist classes are user_symbols of type IndexContainer
  ForEach(
    const Element& pos,
    class StatementBlock *parentBlock, 
    const Symbol& iter,
    const IdList& in,
    const IdList& excl,
    const IdList& order,
    bool  orderIsAscending); 

 // MANIPULATORS

  void executeBlock();
  void print(InfoScript& i)const;
  const IndexParameter* currentIndex() const;
  bool isForEachBlock() const;
};

}

#endif
