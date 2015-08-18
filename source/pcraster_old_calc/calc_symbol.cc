#include "stddefx.h"

#ifndef INCLUDED_CALC_SYMBOL
#include "calc_symbol.h"
#define INCLUDED_CALC_SYMBOL
#endif

#ifndef INCLUDED_CALC_EXTSYM
#include "calc_extsym.h"
#define INCLUDED_CALC_EXTSYM
#endif

#ifndef INCLUDED_LEXTOKEN
#include "lextoken.h"
#define INCLUDED_LEXTOKEN
#endif

#ifndef INCLUDED_COM_STRCONV
#include "com_strconv.h"
#define INCLUDED_COM_STRCONV
#endif

//! emtpy ctor
calc::Symbol::Symbol()
 : Element()
{
}

//! dtor
calc::Symbol::~Symbol()
{
}

//! parser/application generated symbol
calc::Symbol::Symbol(IScript *script, const std::string& name,
                    const Position *pos):
 Element(script,pos),
 d_name(name)
{
}

//! parser/application generated symbol
calc::Symbol::Symbol(IScript *script, const ExtSym& s):
 Element(script,s.position()),
 d_name(s.name())
{
}

//! ctor
calc::Symbol::Symbol(IScript *script, const LexToken* token):
 Element(script,token->position()),
 d_name(token->stringVal())
{
  DEVELOP_PRECOND(!d_name.empty());
}

calc::Symbol::Symbol(const Element& e, const std::string& name):
 Element(e),
 d_name(name)
{
  DEVELOP_PRECOND(!d_name.empty());
}

//! set name
void calc::Symbol::setName(const std::string& newName)
{
  DEVELOP_PRECOND(!newName.empty());
  d_name=newName;
}

//! name of symbol
/*! in the case of a BindedSymbol this is the
    name as used in the script as model parameter
 */
const std::string& calc::Symbol::name() const
{
  if (d_name.empty())
    throw SyntaxErrorBug();
  DEVELOP_PRECOND(!d_name.empty());
  return d_name;
}

//! as name(), surrounded with single quotes
std::string calc::Symbol::qName() const
{
 return quote(name());
}

//! used in parser only
bool calc::Symbol::empty() const
{
  return d_name.empty();
}

bool calc::Symbol::isNumber() const
{
  try {
    toNumber();
  } catch (...) {
    return false;
  }
  return true;
}

double calc::Symbol::toNumber() const
{
  return com::fromString<double>(name());
}


bool calc::operator<(const Symbol& lhs, const Symbol& rhs)
{
  return lhs.name() < rhs.name();
}
bool calc::operator==(const Symbol& lhs, const Symbol& rhs)
{
  return lhs.name() == rhs.name();
}
