#include "stddefx.h"
#include "com_exception.h"
#include "calc_lookuptableparameter.h"
#include "calc_lookuptable.h"
#include "calc_indextable.h"
#include "calc_iscript.h"
#include "calc_newxmldatasubtype.h"
#include <sstream>

calc::LookupTableParameter::LookupTableParameter(
  const ParsPar& par,
  const IndexTable *table):
  SubParameter(par,true,true), d_table(table)
{
}

calc::LookupTableParameter::LookupTableParameter(
  const calc::ParsPar& par,
  const std::vector<class LookupTable *>& val):
  SubParameter(par,true,true), d_vals(val),d_table(nullptr)
{
}

calc::LookupTableParameter::~LookupTableParameter()
{
  for(auto & d_val : d_vals)
    delete d_val;
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
