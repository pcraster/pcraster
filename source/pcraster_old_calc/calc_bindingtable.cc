#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_BINDINGTABLE
#include "calc_bindingtable.h"
#define INCLUDED_CALC_BINDINGTABLE
#endif

// Library headers.
#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
#endif
// PCRaster library headers.

// Module headers.

#ifndef INCLUDED_CALC_EXTSYM
#include "calc_extsym.h"
#define INCLUDED_CALC_EXTSYM
#endif
#ifndef INCLUDED_CALC_RUNDIRECTORY
#include "calc_rundirectory.h"
#define INCLUDED_CALC_RUNDIRECTORY
#endif
#ifndef INCLUDED_CALC_FIELDNRPARAMETER
#include "calc_fieldnrparameter.h"
#define INCLUDED_CALC_FIELDNRPARAMETER
#endif
#ifndef INCLUDED_CALC_USEPAR
#include "calc_usepar.h"
#define INCLUDED_CALC_USEPAR
#endif


/*!
  \file
  This file contains the implementation of the BindingTable class.
*/



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

const calc::Symbol* calc::BindingTable::find(const std::string& name) const
{
    calc::Symbol s(0,name,0);
    Table::const_iterator p=d_table.find(s);
    if (p != d_table.end())
        return &(p->second.d_value);
    return 0;
}

void calc::BindingTable::setExternalBindings(
    IScript            *addToThis,
    const RunDirectory& rd)
{
  const std::map<ExtSym,ExtSym>& extB(rd.bindings());
  std::map<ExtSym,ExtSym>::const_iterator i;

  for(i=extB.begin(); i!=extB.end(); ++i) {
    // not yet there
    DEVELOP_PRECOND(!find(i->first.name()));
    d_table.insert(std::make_pair(
          Symbol(addToThis, i->first),
          Right(External,Symbol(addToThis, i->second),VS_FIELD)));
  }
}


std::vector<calc::UserSymbol *>
 calc::BindingTable::moveConstantToParameters(
    StatementBlock *block)
{
  std::vector<UserSymbol *> newPars;
  for(Table::iterator i=d_table.begin(); 
      i!=d_table.end(); ++i) {
    const Right& r(i->second);
    if (r.d_value.isNumber()) {
      newPars.push_back(
       new FieldNrParameter(UsePar(block,i->first), r.d_value.toNumber(),r.d_vs));
    }
  }
  for(std::vector<UserSymbol *>::iterator i=newPars.begin(); 
      i!=newPars.end(); ++i)
       d_table.erase(**i);
  return newPars;
}

void calc::BindingTable::add(const Symbol& left, const Symbol& right, VS vs)
{
  //! if right is a binding we want to copy it's contents
  const Symbol* rightV = find(right.name());
  if (!rightV)
      rightV = &right;

  std::pair<Table::iterator,bool> p=d_table.insert(std::make_pair(
         left,
         Right(InScript,*rightV,vs)));
  Table::const_iterator fd =p.first;
        // first definition if duplicate, or new one if not
  if (!p.second) // duplicate: this is old one
     if (fd->second.d_definitionLevel == InScript) {
       // pcrcalc/test43[ab]
       std::ostringstream msg;
       msg << left.qName() << " is used twice as binding name, first use at "
           << fd->first.definitionPoint();
       left.posError(msg);
     }
}

calc::BindingTable::Right::Right(
    DefinitionLevel  definitionLevel,
    const Symbol& value,
    VS            vs):
    d_definitionLevel(definitionLevel),
    d_value(value),
    d_vs(vs)
{
}


//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



