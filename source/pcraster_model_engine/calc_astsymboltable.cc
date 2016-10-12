#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_ASTSYMBOLTABLE
#include "calc_astsymboltable.h"
#define INCLUDED_CALC_ASTSYMBOLTABLE
#endif

// Library headers.
#ifndef INCLUDED_BOOST_FOREACH
#include <boost/foreach.hpp>
#define INCLUDED_BOOST_FOREACH
#endif
// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_CALC_ASTPAR
#include "calc_astpar.h"
#define INCLUDED_CALC_ASTPAR
#endif
#ifndef INCLUDED_CALC_BINDINGTABLE
#include "calc_bindingtable.h"
#define INCLUDED_CALC_BINDINGTABLE
#endif
#ifndef INCLUDED_CALC_SYMEXCEPTION
#include "calc_symexception.h"
#define INCLUDED_CALC_SYMEXCEPTION
#endif
#ifndef INCLUDED_CALC_LINKINLIBRARY
#include "calc_linkinlibrary.h"
#define INCLUDED_CALC_LINKINLIBRARY
#endif

/*!
  \file
  This file contains the implementation of the ASTSymbolTable class.
*/



//------------------------------------------------------------------------------

/*
namespace calc {

class ASTSymbolTablePrivate
{
public:

  ASTSymbolTablePrivate()
  {
  }

  ~ASTSymbolTablePrivate()
  {
  }

};

} // namespace calc
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC ASTSYMBOLTABLE MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF ASTSYMBOLTABLE MEMBERS
//------------------------------------------------------------------------------

calc::ASTSymbolTable::ASTSymbolTable()
{
}

calc::ASTSymbolTable::ASTSymbolTable(Base const& syms):
  Base(syms)
{
}

calc::ASTSymbolTable::~ASTSymbolTable()
{
}


//! as std::map::operator[] also inserts p if unknown
/*!
 * this is the only method where symbols are inserted in the table in
 * addition to insert
 */
calc::ASTSymbolInfo& calc::ASTSymbolTable::operator[](const ASTPar *p) {
  if (!(contains(p->name())))
    Base::insert(std::make_pair(p->name(),ASTSymbolInfo(p->returnDataType(),p)));
  PRECOND(contains(p->name()));
  return Base::operator[](p->name());
}

const calc::ASTSymbolInfo& calc::ASTSymbolTable::operator[](const ASTPar *p) const {
  PRECOND(contains(p->name()));
  const_iterator f=find(p->name());
  return f->second;
}

const calc::ASTSymbolInfo& 
  calc::ASTSymbolTable::operator[](const std::string& name) const
{
  PRECOND(contains(name));
  const_iterator f=find(name);
  return f->second;
}

bool calc::ASTSymbolTable::contains(const std::string& name) const
{
  const_iterator f=find(name);
  return f !=end();
}

bool calc::ASTSymbolTable::contains(const ASTPar *p) const
{
  return contains(p->name());
}

void calc::ASTSymbolTable::throwSym(const SymException& s) const
{
  const_iterator f=find(s.symbolName());
  if (f==end()) // not in table
    s.throwPos(s.symbolName());
  f->second.throwSym(s);
}

/*! check that each symbol has a different external name
 *
 *  \todo
 *    We now check that the full set (input+output) has no
 *    duplicates. What we really want is:
 *    - input set may have duplicates
 *    - output set may not have duplicates
 *    - input and output set may not have an intersection
 */
void calc::ASTSymbolTable::checkDifferentExternalNames() const
{
  typedef std::string ExternalName;
  typedef std::string SymbolNameWithThatExternalName;
  typedef std::map<ExternalName, SymbolNameWithThatExternalName> CheckMap;
  CheckMap checked;

  const calc::ASTSymbolTable& this_(*this);
  BOOST_FOREACH(ASTSymbolTablePair i, this_) {
    const ASTSymbolInfo& s(i.second);
    CheckMap::const_iterator dup=checked.find(s.externalName());
    if (dup!=checked.end()) {
      std::ostringstream str;
      str << "shares identical binding with '" << dup->second <<
             "':" << s.externalName();
      s.throwAtFirst(com::Exception(str.str()));
    }
    checked.insert(std::make_pair(s.externalName(),s.name()));
  }
}

bool calc::ASTSymbolTable::containsMemoryExchangeSymbols() const
{
  const size_t noExchange(ASTSymbolInfo::noMemoryExchangeId());
  const calc::ASTSymbolTable& this_(*this);
  BOOST_FOREACH(ASTSymbolTablePair i, this_) {
    ASTSymbolInfo const& si(i.second);
    if (si.memoryInputId() != noExchange || si.memoryOutputId() != noExchange)
       return true;
  }
  return false;
}

//! find or insert new library
/*!
   \throws LinkInLibraryException in case of error
 */
calc::LinkInLibrary const* calc::ASTSymbolTable::linkInLibrary(std::string const& name)
{
  LinkInLibraries::const_iterator i=d_linkInLibraries.find(name);
  if (i!=d_linkInLibraries.end())
    return i->second.get();
  else {
   try {
    d_linkInLibraries.insert(std::make_pair(name,
      boost::shared_ptr<LinkInLibrary>(new LinkInLibrary(name))));
   } catch (com::Exception const &e) {
     LinkInLibraryException l;
     l.message=e.messages();
     throw l;
   }
   i=d_linkInLibraries.find(name);
   return i->second.get();
  }
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------

std::ostream& calc::operator<<(std::ostream& s, const calc::ASTSymbolTable& t)
{
  s << "\n";
  BOOST_FOREACH(ASTSymbolTablePair pos, t) {
    s << "name(" << pos.first  << ")";
    s << "info(" << pos.second << ")\n";
  }
  return s;
}


//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------


extern "C" const char *interfaceAsXML()
{
  static const char *xml = "<xml>vos</xml>";
  return xml;
}

