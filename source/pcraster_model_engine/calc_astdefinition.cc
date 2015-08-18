#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_ASTDEFINITION
#include "calc_astdefinition.h"
#define INCLUDED_CALC_ASTDEFINITION
#endif

// Library headers.
#ifndef INCLUDED_SET
#include <set>
#define INCLUDED_SET
#endif
#ifndef INCLUDED_BOOST_FORMAT
#include <boost/format.hpp>
#define INCLUDED_BOOST_FORMAT
#endif

// PCRaster library headers.
#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif

// Module headers.
#ifndef INCLUDED_CALC_DIMENSION
#include "calc_dimension.h"
#define INCLUDED_CALC_DIMENSION
#endif



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
void calc::ASTDefinition::setName(const Id& name)
{
  d_name=name;
}

//! get value of d_name
const std::string& calc::ASTDefinition::name() const
{
  return d_name.name();
}

void calc::ASTDefinition::add(const Id& key, const Id& value)
{
  KeyValue::const_iterator i = d_items.find(key);
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
    if (!d_definitionRole.empty())
      key.symError(
        (boost::format("can not define both %1% and %2%")
         % d_definitionRole % key.name()).str());
    d_definitionRole = key.name();
  }

  // add non-definitionrole keys
  keys.insert("unit");
  keys.insert("description");

  // known key?
  if (!keys.count(key.name()))
    key.symError("unknown item");

  if (key.name()=="unit") {
    try {
      Dimension dim(value.name());
    } catch(const com::Exception& e) {
      value.symError(e.messages());
    }
  }

  d_items.insert(std::make_pair(key,value));
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
  KeyValue::const_iterator i = d_items.find(TmpId("unit"));
  if (i != d_items.end())
    return Dimension(i->second());
  return Dimension();
}

//! return empty if not there
std::string calc::ASTDefinition::description() const
{
  KeyValue::const_iterator i = d_items.find(TmpId("description"));
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



