#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_BINDINGTABLE
#include "calc_bindingtable.h"
#define INCLUDED_CALC_BINDINGTABLE
#endif

// Library headers.
#ifndef INCLUDED_MAP
#include <map>
#define INCLUDED_MAP
#endif
#ifndef INCLUDED_SET
#include <set>
#define INCLUDED_SET
#endif
// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_CALC_ASTVISITOR
#include "calc_astvisitor.h"
#define INCLUDED_CALC_ASTVISITOR
#endif
#ifndef INCLUDED_CALC_ASTSYMBOLTABLE
#include "calc_astsymboltable.h"
#define INCLUDED_CALC_ASTSYMBOLTABLE
#endif
#ifndef INCLUDED_CALC_ASTPAR
#include "calc_astpar.h"
#define INCLUDED_CALC_ASTPAR
#endif
#ifndef INCLUDED_CALC_ASTASS
#include "calc_astass.h"
#define INCLUDED_CALC_ASTASS
#endif

/*!
  \file
  This file contains the implementation of the BindingTable class.
*/

namespace calc {

typedef std::set<std::string> StringSet;

class BindingToSymbol: public ASTVisitor {

private:

  //! Assignment operator. NOT IMPLEMENTED.
  BindingToSymbol&           operator=           (const BindingToSymbol&);

  //! Copy constructor. NOT IMPLEMENTED.
                   BindingToSymbol               (const BindingToSymbol&);

  //! table that is updated
  ASTSymbolTable&                          d_symbols;
  const StringSet&                         d_interfaceSyms;

  typedef std::map<std::string, const ASTAss *> Defined;
  //! maps name to "left=right" syntax construct in ASTAss node
  Defined                                  d_defined;

  ASTAss*                                  d_currentAss;

  void visitPar          (ASTPar    *p);
  void visitNumber       (ASTNumber *n);
  void visitAss          (ASTAss    *a);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  BindingToSymbol(ASTSymbolTable& symbols,const StringSet& interfaceSyms):
    d_symbols(symbols),
    d_interfaceSyms(interfaceSyms)
    {}

   ~BindingToSymbol()
    {}

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
};

void BindingToSymbol::visitAss(ASTAss *a) {
  const ASTPar *par(a->par());

  Defined::const_iterator i= d_defined.find(par->name());
  if (i != d_defined.end()) {
    // pcrcalc43[ab]
    std::ostringstream msg;
    msg << par->qName() << " is used twice as binding name, first use at "
        << i->second->par()->shortPosText();
    par->posError(msg);
  }
  if (d_interfaceSyms.count(par->name()))
    par->symError("binding is obsolete when using an interface section");

  // evaluate iff binding is used in d_symbols
  if (d_symbols.contains(par)) {
    d_currentAss = a;
    // visitPar or visitNumber:
    a->rhs()->accept(*this);
  }
  d_defined.insert(std::make_pair(par->name(),a));

}

void BindingToSymbol::visitPar(ASTPar    *rhs)
{
  Defined::const_iterator i= d_defined.find(rhs->name());
  if (i != d_defined.end()) {
    // bindings use each other: overwrite rhs
    d_currentAss->transferRhs(i->second->rhs()->createClone());
    d_currentAss->rhs()->accept(*this);
  } else {
    ASTSymbolInfo& s(d_symbols[d_currentAss->par()]);
    s.setExternalNameByBinding(rhs->name());
  }
}

void BindingToSymbol::visitNumber(ASTNumber *n)
{
  d_symbols[d_currentAss->par()].setConstantByBinding(n);
}

} // eo namespace

//------------------------------------------------------------------------------
// DEFINITION OF STATIC BINDINGTABLE MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF BINDINGTABLE MEMBERS
//------------------------------------------------------------------------------

calc::BindingTable::BindingTable()
{
}

calc::BindingTable::~BindingTable()
{
}

calc::EffectiveBindings::EffectiveBindings(BindingTable const& bt):
  BindingTable(bt)
{
}

//! apply bindings to \a t
/*!
 * Only the bindings that have a rhs present in the ASTSymbolTable \a t
 * are evaluated.
 * This object is modified if the bindings use each other.
 * Throws for multiple definitions, apply checks on the script binding
 * where only 1 binding per symbol is allowed.
 * \param t  symbol table that is updated
 * \param interfaceSyms  symbol present in interface, if in interface then
 *                       a binding is illegal, only used for this check
 * \throws DataTypeClash if a binding type clashes with entry in \a t
 *         t is modified such that a new buildTypesFullClosure call will generate
 *         an error.
 */
void calc::EffectiveBindings::applyToSymbols(
    ASTSymbolTable& t,
    const std::set<std::string>& interfaceSyms)
{
  BindingToSymbol bt(t,interfaceSyms);
  accept(bt);
}

//! e is the list of external bindings, that overwrite existing bindings
void calc::EffectiveBindings::overwrite(
 const ASTNodeVector& e)
{
 ASTNodeVector l;
 for(const_iterator i=begin(); i!=end(); ++i)
   l.transferPushBack((*i)->createClone());
 for(const_iterator i=e.begin(); i!=e.end(); ++i)
   l.transferPushBack((*i)->createClone());
 clear();
 addLastDefinition(l);
}

//! copy the last definitions occuring in \a l
/*! \a l has a series of definitions, the last occuring
 *  definition for a certain symbol must be copied into
 *  this, while retaining the order occurence
 *
 *  addLastDefinition is in support of the external bindings
 *  file definitions, (see RunSettings ctor)
 */
void calc::BindingTable::addLastDefinition(
    const ASTNodeVector &l)
{
  // do keep order of occurences

  std::set<std::string> names;
  std::vector<ASTAss *> add;
  size_t i=l.size();
  while(i) {
    --i;
    ASTAss *a= dynamic_cast<ASTAss *>(l[i]);
    std::string name(a->par()->name());
    if (!names.count(name)) {
      add.push_back(new ASTAss(*a));
      names.insert(name);
    }
  }
  i=add.size();
  while(i) {
    --i;
    transferPushBack(add[i]);
  }
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------
