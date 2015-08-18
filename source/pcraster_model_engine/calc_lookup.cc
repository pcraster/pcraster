#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_LOOKUP
#include "calc_lookup.h"
#define INCLUDED_CALC_LOOKUP
#endif

// Library headers.

// PCRaster library headers.
#ifndef INCLUDED_COM_CSFCELL
#include "com_csfcell.h"
#define INCLUDED_COM_CSFCELL
#endif

// Module headers.
#ifndef INCLUDED_CALC_EXECARGUMENTS
#include "calc_execarguments.h"
#define INCLUDED_CALC_EXECARGUMENTS
#endif
#ifndef INCLUDED_CALC_LOOKUPTABLE
#include "calc_lookuptable.h"
#define INCLUDED_CALC_LOOKUPTABLE
#endif
#ifndef INCLUDED_CALC_FIELD
#include "calc_field.h"
#define INCLUDED_CALC_FIELD
#endif

#ifndef INCLUDED_CALC_DOMAINERROR
#include "calc_domainerror.h"
#define INCLUDED_CALC_DOMAINERROR
#endif


/*!
  \file
  This file contains the implementation of the Lookup class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC LOOKUP MEMBERS
//------------------------------------------------------------------------------

namespace calc {

namespace lookup {

struct Interpolate {
    bool operator()(
      const LookupTable *tab,
      double& result,
      const LookupTable::Key& key)
    {
      return tab->interpolate(result, key);
    }
};

struct Find {
    bool operator()(
      const LookupTable *tab,
      double& result,
      const LookupTable::Key& key)
    {
      return tab->find(result, key);
    }
};

template<typename F>
static void execLookupLookup(
    ExecArguments& a,
    F              f)
{

  const calc::LookupTable *tab=
    dynamic_cast<const calc::LookupTable *>(a.firstNonFieldInput());
  PRECOND(tab);

  Field& r(a.createResult());

  size_t nrKeys(a.size()); // = size of fields
  LookupTable::Key keyValues(nrKeys);

  size_t nr(r.nrValues());

  try {
    for(size_t i=0; i < nr; i++) {
      double res;
      pcr::setMV(res); // if goto is exec'ed then ok value
      for(size_t k = 0; k < nrKeys; k++) {
        double keyVal;
        if (!a[k].getCell(keyVal, i))
          goto store;
        keyValues[k]=(RelationRecord::Float)keyVal;
      }
      f(tab,res, keyValues);
      store:
        r.setCell(res, i);
    }
  } catch(DomainError& ) {
     // set a NonSpatial to MV
     // pcrcalc225
     throw DomainError("No match");
  }
  a.pushResults();
}
}

Lookup builtIn_lookupnominal;
Lookup builtIn_lookupboolean;
Lookup builtIn_lookupordinal;
Lookup builtIn_lookupscalar;
Lookup builtIn_lookupdirectional;
Lookup builtIn_lookupldd;
LookupLinear builtIn_lookuplinear;

}



//------------------------------------------------------------------------------
// DEFINITION OF LOOKUP MEMBERS
//------------------------------------------------------------------------------

calc::Lookup::Lookup()
{
}

calc::Lookup::~Lookup()
{
}

void calc::Lookup::exec(RunTimeEnv* rte,const Operator& op,size_t nrArgs) const
{
  ExecArguments a(op,rte,nrArgs);
  lookup::execLookupLookup(a,lookup::Find());
}

calc::LookupLinear::LookupLinear()
{
}

calc::LookupLinear::~LookupLinear()
{
}

void calc::LookupLinear::exec(RunTimeEnv* rte,const Operator& op,size_t nrArgs) const
{
  ExecArguments a(op,rte,nrArgs);
  lookup::execLookupLookup(a,lookup::Interpolate());
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



