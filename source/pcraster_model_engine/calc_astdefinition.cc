#include "stddefx.h"
#include "calc_astdefinition.h"
#include "com_exception.h"
#include "calc_dimension.h"

#include <format>
#include <set>

/*!
  \file
  This file contains the implementation of the ASTDefinition class.
*/


//------------------------------------------------------------------------------
// DEFINITION OF STATIC ASTDEFINITION MEMBERS
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// DEFINITION OF ASTDEFINITION MEMBERS
//------------------------------------------------------------------------------

calc::ASTDefinition::ASTDefinition()
{
}

/* default
//! Copy constructor.
calc::ASTDefinition::ASTDefinition(ASTDefinition const& rhs)

  : Base(rhs)

{
}
*/


calc::ASTDefinition::~ASTDefinition()
{
}

/* default
//! Assignment operator.
calc::ASTDefinition& calc::ASTDefinition::operator=(ASTDefinition const& rhs)
{
  if (this != &rhs) {
  }
  return *this;
}
*/

//! set value of d_name
void calc::ASTDefinition::setName(const Id &name)
{
  d_name = name;
}

//! get value of d_name
const std::string &calc::ASTDefinition::name() const
{
  return d_name.name();
}

void calc::ASTDefinition::add(const Id &key, const Id &value)
{
  auto i = d_items.find(key);
  if (i != d_items.end())
    key.symError("redefinition of item");

  std::set<std::string> keys;
  // mutual exclusive definitionroles
  keys.insert("input");
  keys.insert("output");
  keys.insert("constant");

  // is it an definitionrole?
  if (keys.count(key.name())) {
    // already set?
    if (!d_definitionRole.empty()) {
      key.symError(std::vformat("can not define both {0} and {1}",
                                std::make_format_args(d_definitionRole, key.name())));
    }
    d_definitionRole = key.name();
  }

  // add non-definitionrole keys
  keys.insert("unit");
  keys.insert("description");

  // known key?
  if (!keys.count(key.name()))
    key.symError("unknown item");

  if (key.name() == "unit") {
    try {
      Dimension const dim(value.name());
    } catch (const com::Exception &e) {
      value.symError(e.messages());
    }
  }

  d_items.insert(std::make_pair(key, value));
}

//! get value of d_definitionRole
calc::DefinitionRole calc::ASTDefinition::definitionRole() const
{
  if (d_definitionRole.empty())
    return NotSpecified;
  return (DefinitionRole)d_definitionRole[0];
}

calc::Dimension calc::ASTDefinition::unit() const
{
  auto i = d_items.find(TmpId("unit"));
  if (i != d_items.end())
    return Dimension(i->second());
  return {};
}

//! return empty if not there
std::string calc::ASTDefinition::description() const
{
  auto i = d_items.find(TmpId("description"));
  if (i != d_items.end())
    return i->second();
  return "";
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------
