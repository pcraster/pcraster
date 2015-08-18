#include "stddefx.h"

#ifndef INCLUDED_CALC_INDEXSELECTEDVECTOR
#include "calc_indexselectedvector.h"
#define INCLUDED_CALC_INDEXSELECTEDVECTOR
#endif

#ifndef INCLUDED_CALC_ARRAYDEFINITION
#include "calc_arraydefinition.h"
#define INCLUDED_CALC_ARRAYDEFINITION
#endif


#ifndef INCLUDED_CALC_INDEXPARAMETER
#include "calc_indexparameter.h"
#define INCLUDED_CALC_INDEXPARAMETER
#endif

#ifndef INCLUDED_CALC_INDEXPARAMETERCONSTANT
#include "calc_indexparameterconstant.h"
#define INCLUDED_CALC_INDEXPARAMETERCONSTANT
#endif

//! factor an ArrayDefVector
calc::ArrayDefVector calc::IndexSelectedVector::arrayDefVector() const
{
    std::vector<const calc::ArrayDefinition *>vector;
    for (size_t i=0; i < size(); i++)
      vector.push_back(d_vector[i]->partOf());
    return calc::ArrayDefVector(vector);
}

calc::IndexSelectedVector::IndexSelectedVector(
  std::vector<const calc::IndexParameter *>vector):
  d_vector(vector)
{
}

calc::IndexSelectedVector::~IndexSelectedVector()
{
}


int calc::IndexSelectedVector::select() const
{
  if (!size())
    return 0;
  std::vector<size_t> ind(size());
  for(size_t i=0; i < d_vector.size(); i++)
    ind[i] = d_vector[i]->index();
  return arrayDefVector().toLinear(ind);
}

//! name of current invocation
std::string calc::IndexSelectedVector::selectedName() const
{
  std::string str = "";
  std::vector<size_t> ind(size());
  for(size_t i=0; i < d_vector.size(); i++)
    str += "["+d_vector[i]->indexParameterConstant()->name() + "]";
  return str;
}
//! name of current invocation
std::string calc::IndexSelectedVector::variableName() const
{
  std::string str = "";
  std::vector<size_t> ind(size());
  for(size_t i=0; i < d_vector.size(); i++)
    str += "["+d_vector[i]->name() + "]";
  return str;
}


std::string calc::IndexSelectedVector::arrayDefName() const
{
  return arrayDefVector().name();
}

const calc::IndexParameter *calc::IndexSelectedVector::operator[]( size_t n) const
{
  PRECOND(d_vector.size() > n);
  return d_vector[n];
}

bool calc::IndexSelectedVector::equal(const calc::IndexSelectedVector *index2) const
{
  PRECOND(size() == index2->size());
  for(size_t i=0; i < d_vector.size(); i++) {
    if (d_vector[i] != (*index2)[i])
      return false;
  }
  return true;
}
