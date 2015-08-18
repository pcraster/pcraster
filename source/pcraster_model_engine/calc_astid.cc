#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_ASTID
#include "calc_astid.h"
#define INCLUDED_CALC_ASTID
#endif

// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_CALC_QUOTE
#include "calc_quote.h"
#define INCLUDED_CALC_QUOTE
#endif
#ifndef INCLUDED_COM_STRCONV
#include "com_strconv.h"
#define INCLUDED_COM_STRCONV
#endif
#ifndef INCLUDED_CALC_ID
#include "calc_id.h"
#define INCLUDED_CALC_ID
#endif


/*!
  \file
  This file contains the implementation of the ASTId class.
*/



//------------------------------------------------------------------------------

/*
namespace calc {

class ASTIdPrivate
{
public:

  ASTIdPrivate()
  {
  }

  ~ASTIdPrivate()
  {
  }

};

} // namespace calc
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC ASTID MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF ASTID MEMBERS
//------------------------------------------------------------------------------

calc::ASTId::ASTId()
{
  setNrReturns(1);
}

calc::ASTId::ASTId(const std::string& name):
  d_name(name)
{
  setNrReturns(1);
}

calc::ASTId::ASTId(const Id& id):
  d_name(id.name())
{
  setPosition(id.position());
  setNrReturns(1);
}

calc::ASTId::~ASTId()
{
}

//! Assignment operator.
calc::ASTId& calc::ASTId::operator=(const ASTId& rhs)
{
  if (this != &rhs) {
    setName(rhs.name());
    setPosition(rhs.position());
  }
  return *this;
}

//! Copy constructor.
calc::ASTId::ASTId(const ASTId& rhs):
    ASTNode(rhs),
    d_name(rhs.d_name)
{
}

//! set name
void calc::ASTId::setName(const std::string& newName)
{
  // When turning on some syntax errors generate failure:
  // DEVELOP_PRECOND(!newName.empty());
  d_name=newName;
}

//! name of symbol
const std::string& calc::ASTId::name() const
{
  // When turning on some syntax errors generate failure:
  // DEVELOP_PRECOND(!d_name.empty());
  return d_name;
}

//! as name(), surrounded with single quotes
std::string calc::ASTId::qName() const
{
 return quote(name());
}

bool calc::ASTId::isNumber() const
{
  return com::isDouble(name());
}

double calc::ASTId::toNumber() const
{
  return com::fromString<double>(name());
}

//! only for debug purposes to check if already initialized with a name
bool calc::ASTId::debugOnlyValid() const
{
  return !d_name.empty();
}

bool calc::operator<(const ASTId& lhs, const ASTId& rhs)
{
  return lhs.name() < rhs.name();
}

bool calc::operator==(const ASTId& lhs, const ASTId& rhs)
{
  return lhs.name() == rhs.name();
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



