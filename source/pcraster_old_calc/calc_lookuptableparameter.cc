#include "stddefx.h"

#ifndef INCLUDED_SSTREAM
#include <sstream>
#define INCLUDED_SSTREAM
#endif

#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif

#ifndef INCLUDED_CALC_LOOKUPTABLEPARAMETER
#include "calc_lookuptableparameter.h"
#define INCLUDED_CALC_LOOKUPTABLEPARAMETER
#endif

#ifndef INCLUDED_CALC_LOOKUPTABLE
#include "calc_lookuptable.h"
#define INCLUDED_CALC_LOOKUPTABLE
#endif

#ifndef INCLUDED_CALC_INDEXTABLE
#include "calc_indextable.h"
#define INCLUDED_CALC_INDEXTABLE
#endif

#ifndef INCLUDED_CALC_ISCRIPT
#include "calc_iscript.h"
#define INCLUDED_CALC_ISCRIPT
#endif

#ifndef INCLUDED_CALC_NEWXMLDATASUBTYPE
#include "calc_newxmldatasubtype.h"
#define INCLUDED_CALC_NEWXMLDATASUBTYPE
#endif

calc::LookupTableParameter::LookupTableParameter(
  const ParsPar& par,
  const IndexTable *table):
  SubParameter(par,true,true), d_table(table)
{
}

calc::LookupTableParameter::LookupTableParameter(
  const calc::ParsPar& par,
  const std::vector<class LookupTable *>& val):
  SubParameter(par,true,true), d_vals(val),d_table(0)
{
}

calc::LookupTableParameter::~LookupTableParameter()
{
  for(size_t i=0; i < d_vals.size(); i++)
    delete d_vals[i];
}

class calc::LookupTable *calc::LookupTableParameter::value(size_t i)
{
  PRECOND(d_vals.size() > i);
  return d_vals[i];
}

VS calc::LookupTableParameter::symbolType() const
{
  return VS_TABLE;
}

void calc::LookupTableParameter::loadValuesFromIndexTable(
  VS result,
  const std::vector<VS>& readKeys)
{
  // already loaded
  if (d_vals.size() > 0)
    return;
  PRECOND(d_table);
  size_t n  = nrElements();
  std::vector<const IndexTable::Value *> tableNames;
  d_table->nameValues(*this,tableNames);
  for(size_t i=0; i < n; i++) {
    const IndexTable::Value *t = tableNames[i];
    try {
      d_vals.push_back(new LookupTable(result));
      d_vals.back()->setRecords(
          scriptConst().inputFilePath(t->d_value),readKeys);
    } catch ( com::Exception& msg) { // pcrcalc/test28[78]
       std::ostringstream newMsg;
       newMsg << msg.messages();
       newMsg << " read from ";
       newMsg << d_table->externalName();
       newMsg << " line ";
       newMsg << t->d_lineNr;
       throw com::Exception(newMsg.str());
   }
  }
}

void calc::LookupTableParameter::setDataSubType(pcrxml::Data *d) const
{
  d->table = newDataSubType<pcrxml::Table>(d_vals[0]->returnVs());
}
