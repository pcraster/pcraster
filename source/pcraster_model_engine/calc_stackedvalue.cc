#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_STACKEDVALUE
#include "calc_stackedvalue.h"
#define INCLUDED_CALC_STACKEDVALUE
#endif

// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_CALC_RUNTIMEENV
#include "calc_runtimeenv.h"
#define INCLUDED_CALC_RUNTIMEENV
#endif
#ifndef INCLUDED_CALC_ASTSYMBOLINFO
#include "calc_astsymbolinfo.h"
#define INCLUDED_CALC_ASTSYMBOLINFO
#endif
/*!
  \file
  This file contains the implementation of the StackedValue class.
*/



//------------------------------------------------------------------------------

/*

class StackedValuePrivate
{
public:

  StackedValuePrivate()
  {
  }

  ~StackedValuePrivate()
  {
  }

};

} // namespace calc
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC STACKEDVALUE MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF STACKEDVALUE MEMBERS
//------------------------------------------------------------------------------

calc::StackedValue::StackedValue(
          RunTimeEnv&   rte,
          const ASTSymbolInfo& symbol,
          bool          lastUse):
    d_rte(rte),
    d_symbol(symbol),
    d_lastUse(lastUse)
{
}



calc::StackedValue::~StackedValue()
{
}

/* NOT IMPLEMENTED
//! Assignment operator.
calc::StackedValue& calc::StackedValue::operator=(const StackedValue& rhs)
{
  if (this != &rhs) {
  }
  return *this;
}

//! Copy constructor. NOT IMPLEMENTED.
calc::StackedValue::StackedValue(const StackedValue& rhs):
  Base(rhs)
{
}
*/

calc::DataValue *calc::StackedValue::load() {
  return d_rte.load(d_symbol.name(),d_symbol.externalName(), d_lastUse);
}

calc::OVS calc::StackedValue::ovs() const
{
  return d_symbol.ovs();
}

bool calc::StackedValue::readOnlyReference() const
{
  return !d_lastUse;
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



