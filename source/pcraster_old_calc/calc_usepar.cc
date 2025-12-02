#include "stddefx.h"
#include "calc_usepar.h"
#include "calc_statementblock.h"  // findSymbol only
#include "calc_indexselectedvector.h"
#include "calc_indexselectedsingle.h"
#include "calc_arraydefvector.h"
#include "calc_indexparameter.h"

void calc::UsePar::init()
{
  std::vector<const ArrayDefinition *> v(d_index.size());
  for (size_t i = 0; i < d_index.size(); i++) {
    d_selector[i] =
        dynamic_cast<const IndexParameter *>(d_block->findSymbol(&d_index[i], VS_INDEX, true));
    POSTCOND(d_selector[i]);

    if (!d_selector[i]->isOn())  // pcrcalc/test305
      d_index[i].posError("Index is switched off");
    v[i] = d_selector[i]->partOf();
  }
  d_descriptor = ArrayDefVector(v);
}

calc::UsePar::UsePar(const ConstructPar &p) : ParsPar(p), d_selector(d_index.size())
{
  init();
}

calc::UsePar::UsePar(StatementBlock *block, const Symbol &p) : ParsPar(block, p)
{
  init();
}

calc::UsePar::~UsePar()
{
}

const calc::ArrayDefVector &calc::UsePar::descriptor() const
{
  return d_descriptor;
}

//! factor an index selected vector, caller should delete
calc::IndexSelected *calc::UsePar::createSelector() const
{
  if (!isArray())
    return new calc::IndexSelectedSingle();
  return new calc::IndexSelectedVector(d_selector);
}
