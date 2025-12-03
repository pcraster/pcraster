#include "stddefx.h"
#include "calc_lookup.h"
#include "com_csfcell.h"
#include "calc_execarguments.h"
#include "calc_lookuptable.h"
#include "calc_field.h"
#include "calc_domainerror.h"

#include <cmath>

/*!
  \file
  This file contains the implementation of the Lookup class.
*/


//------------------------------------------------------------------------------
// DEFINITION OF STATIC LOOKUP MEMBERS
//------------------------------------------------------------------------------

namespace calc
{

namespace lookup
{

struct Interpolate {
  bool operator()(const LookupTable *tab, double &result, const LookupTable::Key &key)
  {
    return tab->interpolate(result, key);
  }
};

struct Find {
  bool operator()(const LookupTable *tab, double &result, const LookupTable::Key &key)
  {
    return tab->find(result, key);
  }
};

template <typename F> static void execLookupLookup(ExecArguments &a, F f)
{

  const auto *tab = dynamic_cast<const calc::LookupTable *>(a.firstNonFieldInput());
  PRECOND(tab);

  Field &r(a.createResult());

  size_t const nrKeys(a.size());  // = size of fields
  LookupTable::Key keyValues(nrKeys);

  size_t const nr(r.nrValues());

  try {
    for (size_t i = 0; i < nr; i++) {
      double res = NAN;
      pcr::setMV(res);  // if goto is exec'ed then ok value
      for (size_t k = 0; k < nrKeys; k++) {
        double keyVal = NAN;
        if (!a[k].getCell(keyVal, i)) {
          goto store;
        }
        keyValues[k] = (RelationRecord::Float)keyVal;
      }
      f(tab, res, keyValues);
    store:
      r.setCell(res, i);
    }
  } catch (DomainError &) {
    // set a NonSpatial to MV
    // pcrcalc225
    throw DomainError("No match");
  }
  a.pushResults();
}
}  // namespace lookup

Lookup builtIn_lookupnominal;
Lookup builtIn_lookupboolean;
Lookup builtIn_lookupordinal;
Lookup builtIn_lookupscalar;
Lookup builtIn_lookupdirectional;
Lookup builtIn_lookupldd;
LookupLinear builtIn_lookuplinear;

}  // namespace calc

//------------------------------------------------------------------------------
// DEFINITION OF LOOKUP MEMBERS
//------------------------------------------------------------------------------

calc::Lookup::Lookup()
{
}

calc::Lookup::~Lookup()
{
}

void calc::Lookup::exec(RunTimeEnv *rte, const Operator &op, size_t nrArgs) const
{
  ExecArguments a(op, rte, nrArgs);
  lookup::execLookupLookup(a, lookup::Find());
}

calc::LookupLinear::LookupLinear()
{
}

calc::LookupLinear::~LookupLinear()
{
}

void calc::LookupLinear::exec(RunTimeEnv *rte, const Operator &op, size_t nrArgs) const
{
  ExecArguments a(op, rte, nrArgs);
  lookup::execLookupLookup(a, lookup::Interpolate());
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------
