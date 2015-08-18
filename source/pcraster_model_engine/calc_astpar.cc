#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_ASTPAR
#include "calc_astpar.h"
#define INCLUDED_CALC_ASTPAR
#endif

// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_CALC_ASTVISITOR
#include "calc_astvisitor.h"
#define INCLUDED_CALC_ASTVISITOR
#endif
#ifndef INCLUDED_CALC_SYMEXCEPTION
#include "calc_symexception.h"
#define INCLUDED_CALC_SYMEXCEPTION
#endif

/*!
  \file
  This file contains the implementation of the ASTPar class.
*/



//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// DEFINITION OF STATIC ASTPAR MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF ASTPAR MEMBERS
//------------------------------------------------------------------------------

calc::ASTPar::ASTPar()
{
}

calc::ASTPar::ASTPar(const std::string& name):
  ASTId(name),
  d_lastUse(false)
{
}

calc::ASTPar::ASTPar(const Id&          id):
  ASTId(id),
  d_lastUse(false)
{
}


calc::ASTPar::~ASTPar()
{
}

//! Assignment operator.
calc::ASTPar& calc::ASTPar::operator=(const ASTPar& rhs)
{
  if (this != &rhs) {
    setName(rhs.name());
    setPosition(rhs.position());
    PRECOND(rhs.d_index.empty()); // TODO

    d_lastUse=rhs.d_lastUse;
  }
  return *this;
}

//! Copy constructor.
calc::ASTPar::ASTPar(const ASTPar& rhs):
    ASTId(rhs),
    d_lastUse(rhs.d_lastUse)
{
  PRECOND(rhs.d_index.empty()); // TODO
}

//! throw SymException for this
/*!
 * Note that a SymException thrown by ASTSymbolInfo is "preferred"
 */
void  calc::ASTPar::symError(const std::string& msg) const
{
 throw SymException(*position(),name(),msg);
}

void calc::ASTPar::runtimeError(
    size_t             timeStep,
    const std::string& msg) const
{
  symError(runtimeErrorFmt(timeStep,msg));
}

//! add index
void calc::ASTPar::pushBackIndex(const Id& i)
{
  d_index.push_back(i);
}


void calc::ASTPar::accept(ASTVisitor& v)
{
  v.visitPar(this);
}

const calc::IdList& calc::ASTPar::index() const
{
  return d_index;
}

calc::ASTPar* calc::ASTPar::createClone() const
{
  return new ASTPar(*this);
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------

bool calc::operator<(const ASTPar& lhs, const ASTPar& rhs)
{
  if (lhs.name() == rhs.name()) {
    if (lhs.index().size() != rhs.index().size())
     return lhs.index().size() < rhs.index().size();
    for(size_t i=0;i!=rhs.index().size();++i)
     if (lhs.index()[i] != rhs.index()[i])
      return lhs.index()[i] < rhs.index()[i];
  }
  return lhs.name() < rhs.name();
}


bool calc::operator==(const ASTPar& lhs, const ASTPar& rhs)
{
  if (lhs.name() != rhs.name() || lhs.index() != rhs.index())
    return false;
  for(size_t i=0;i!=rhs.index().size();++i)
    if (lhs.index()[i] != rhs.index()[i])
      return false;
  return true;
}

//! set value of d_lastUse
void calc::ASTPar::setLastUse(bool lastUse)
{
  d_lastUse=lastUse;
}

//! get value of d_lastUse
bool calc::ASTPar::lastUse() const
{
  return d_lastUse;
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



