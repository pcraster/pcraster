#include "stddefx.h"
#include "calc_parspar.h"
#include "calc_constructpar.h"

calc::ParsPar::ParsPar(const ConstructPar &p)
    : BindedSymbol(p.d_name), d_block(p.d_block), d_index(p.d_index)
{
  POSTCOND(p.d_block);
}

calc::ParsPar::ParsPar(StatementBlock *block, const Symbol &p) : BindedSymbol(p), d_block(block)
{
}

bool calc::ParsPar::isArray() const
{
  return !d_index.empty();
}
