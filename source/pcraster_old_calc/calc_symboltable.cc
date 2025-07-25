#include "stddefx.h"
#include "com_algorithm.h"
#include "pcrgenxml_data.h"
#include "calc_symboltable.h"
#include "calc_subparameter.h"
#include "calc_infoscript.h"
#include "calc_parspar.h"
#include "calc_statementblock.h"   // parentBlock->findSymbol
#include "calc_arcviewextcheckdata.h"

calc::SymbolTable::SymbolTable(
  calc::StatementBlock *parentBlock):
    d_parentBlock(parentBlock),
    d_symbolSequenceNr(0)
{
}

calc::SymbolTable::~SymbolTable()
{
 /* can not delete "index containers" yet
  * since when writing/deleting other symbols
  * (e.g. arrayed) parameters we might referer
  * to them
  */
 std::vector<calc::UserSymbol *>indexCont;
 for (auto & p : d_table) {
   calc::UserSymbol *u  = p.second;
  if (! isIn(u->symbolType(), VS_INDEX_CONTAINER))
      delete u;
        else
    indexCont.push_back(u);
 }
 com::forWhole(indexCont,com::Delete<UserSymbol>());
}

void calc::SymbolTable::print(calc::InfoScript& i)const
{
  i.stream() << "<BR>";
  for (const auto & p : d_table) {
    if (p.second->symbolType() != VS_INDEX) {
     p.second->print(i);
     i.stream() << "<HR>";
    }
  }
}

calc::UserSymbol * calc::SymbolTable::find( const std::string& name) const
{
  auto p=d_table.find(name);
  if (p!= d_table.end())
    return p->second;
  return nullptr;
}

calc::UserSymbol *calc::SymbolTable::find(const class calc::Symbol* sym,
    VS typesExpected, bool mustExist) const
{
   calc::UserSymbol *u=find(sym->name());
   if ( (!u) && d_parentBlock)
     u = d_parentBlock->findSymbol(sym, typesExpected,mustExist);
   if ( (!u) && mustExist)
     sym->posError(sym->qName()+" not defined");
   if (u) {
     if (! isIn(u->symbolType(),typesExpected)) {
     // pcrcalc/test265  GPF'ed bcc55/release mode if posError with +'ed
     //  strings, now ok with ostringstream
     // pcrcalc/r11{pre}.res
     // pcrcalc/r2.res
     // pcrcalc/r256.res
     // pcrcalc/r26[567].res
     // pcrcalc/r277.res
     // pcrcalc/r324.res
     // pcrcalc/r328.res
     // pcrcalc/r61.res
     std::ostringstream msg;
      msg << u->qName()
          << " is defined as "
          << toString(u->symbolType())
          << " type on "
          << u->definitionPoint()
          << " and used here as "
          << toString(typesExpected)
          << " type";
      sym->posError(msg);
    }
   }
   return u;
}

//! add symbol to the script's symbol table
/*! SymbolTable owns \a newPar for deletion
    \exception posError if \a newPar is defined twice
 */
void calc::SymbolTable::add(calc::UserSymbol *newPar)
{
 try {
  PRECOND(newPar != nullptr);
  ParameterTableItem addElem(newPar->name(), newPar);
  std::pair<Iter,bool> p=d_table.insert(addElem);
  if(! p.second) {
   // test261 test262 test273a test273 test322
   calc::UserSymbol *firstdef = p.first->second;
   std::ostringstream msg;
   msg << newPar->qName()
       << " defined twice, first definition at "
       << firstdef->definitionPoint();
   newPar->posError(msg);
  }
  newPar->setSymbolSequenceNr(d_symbolSequenceNr++);
 } catch (...) {
   delete newPar;
   throw;
 }
}

void calc::SymbolTable::goInScope()
{
  // TODO
  // std::vector<UserSymbol *> d_t; 
  // com::forWhole(d_t,std::mem_fun(&UserSymbol::goInScope));

  for (auto & p : d_table)
    p.second->goInScope();
}

void calc::SymbolTable::finalCheck()
{
  for (auto & p : d_table)
    p.second->finalCheck();
}

calc::SubParameter *calc::SymbolTable::findParameter(
    const calc::ParsPar&      par,
          VS                 expectedVs,
          bool               mustExist) const
{
  auto *p = dynamic_cast<calc::SubParameter *>
        (find(&par,expectedVs,mustExist));
  if (p && !(par.descriptor() == p->arrayDefVector())) { // pcrcalc/test268
    std::ostringstream msg;
     msg << par.qName()
         << " is used here as "
         << par.name()
         << par.descriptor().name()
         << " first use ("
         << p->definitionPoint()
         << ") was "
         << p->arrayName();
    par.posError(msg);
  }
  return p;
}

calc::SubParameter *calc::SymbolTable::findRightParameter(
    const calc::ParsPar& par,
    VS expectedVs) const
{
  return findParameter(par,expectedVs,par.isArray());
}


calc::SubParameter *calc::SymbolTable::findLeftParameter(
    const calc::ParsPar& par,
    VS expectedVs) const
{
  return findParameter(par,expectedVs,false);
}

/*
static bool lessThan(
    const calc::UserSymbol *u1,
    const calc::UserSymbol *u2)
{
  return u1->symbolSequenceNr() < u2->symbolSequenceNr();
}
*/

//! add Data elements info
void calc::SymbolTable::createXmlData(
    std::vector<pcrxml::Data *>& addHere) const
{
  //  in order of definition
  std::vector<const UserSymbol *> inDefOrder; 
  for (const auto & p : d_table) 
    inDefOrder.push_back(p.second);
//  std::sort(inDefOrder.begin(),inDefOrder.end(),lessThan);

  for (auto & i : inDefOrder) {
      pcrxml::Data *d = i->createXmlData();
      if (d)
        addHere.push_back(d);
  }
}

#include "com_pathname.h"

//! fill r with data that must checked in ArcView interface
/*!
 * \todo check if arc view extension want abs paths
 */
void calc::SymbolTable::setArcViewExtCheckData(
         std::vector<ArcViewExtCheckData>& r) const
{
  std::vector<pcrxml::Data *> d;
  createXmlData(d);
  for(auto & i : d) {
    switch(i->ioType()) {
     case pcrxml::IoType::Output:
     case pcrxml::IoType::Both: {
        if (!(i->stack || i->map))
           break; // only stacks or maps can be in foreign (ArcView) format
        com::PathName pn;
        if (i->externalFileName.present())
             pn=i->externalFileName();
        else
             pn=i->name();
        pn.makeAbsolute();
        r.push_back(ArcViewExtCheckData(i->stack != nullptr,
           pn.toString()));
     }
     default: ;
    }
    delete i;
    i=nullptr;
  }
}
