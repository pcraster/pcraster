#include "stddefx.h"

#ifndef  INCLUDED_CALC_PARSPAR
#include "calc_parspar.h"
#define  INCLUDED_CALC_PARSPAR
#endif

#ifndef INCLUDED_CALC_CONSTRUCTPAR
#include "calc_constructpar.h"
#define INCLUDED_CALC_CONSTRUCTPAR
#endif

calc::ParsPar::ParsPar(const ConstructPar& p)
  :
    BindedSymbol(p.d_name),
    d_block(p.d_block),
    d_index(p.d_index)
{
  POSTCOND(p.d_block);
}

calc::ParsPar::ParsPar(
    StatementBlock *block,
    const Symbol& p):
    BindedSymbol(p),
    d_block(block)
{
}

bool calc::ParsPar::isArray() const
{ return !d_index.empty(); }
