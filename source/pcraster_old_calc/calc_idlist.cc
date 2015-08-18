#include "stddefx.h"

#ifndef INCLUDED_CALC_IDLIST
# include "calc_idlist.h"
#define INCLUDED_CALC_IDLIST
#endif

calc::IdList::IdList()
{}

calc::IdList::IdList(const calc::Symbol& id)
{
  d_idList.push_back(id);
}

calc::IdList::IdList(const std::vector<calc::Symbol>& idList):
  d_idList(idList)
{
}

size_t calc::IdList::size() const
{
  return d_idList.size();
}

const calc::Symbol& calc::IdList::operator[](size_t nr) const
{
  PRECOND(nr < d_idList.size());
  return d_idList[nr];
}
