#include "stddefx.h"

#ifndef INCLUDED_CALC_ARRAYDEFVECTOR
#include "calc_arraydefvector.h"
#define INCLUDED_CALC_ARRAYDEFVECTOR
#endif

#ifndef INCLUDED_CALC_ARRAYDEFINITION
#include "calc_arraydefinition.h"
#define INCLUDED_CALC_ARRAYDEFINITION
#endif

#ifndef INCLUDED_CALC_INDEXPARAMETERCONSTANT
#include "calc_indexparameterconstant.h"
#define INCLUDED_CALC_INDEXPARAMETERCONSTANT
#endif

#ifndef INCLUDED_CALC_INFOSCRIPT
#include "calc_infoscript.h"
#define INCLUDED_CALC_INFOSCRIPT
#endif

calc::ArrayDefVector::ArrayDefVector(
   const std::vector<const class calc::ArrayDefinition* >&vector):
    d_vector(vector)
{
  std::vector<size_t> subElements(d_vector.size());

  size_t nr=1;
  for(int a= d_vector.size()-1; a >= 0; a--) {
    subElements[a] = nr;
    nr*= d_vector[a]->activeIndexSize();
  }

  d_indices.resize(nr);
  for(size_t i=0; i < nr; i++)
    d_indices[i].resize(d_vector.size());
  for(size_t a=0; a < d_vector.size(); a++) {
   size_t pos=0;
   while(pos != nr)
    for(size_t i = 0; i < d_vector[a]->activeIndexSize(); i++)
     for(size_t n=0; n < subElements[a]; n++)
      d_indices[pos++][a] = i;
  }
  for(size_t i=0; i < nr; i++)
    d_map2flatIndex[d_indices[i]] = i;
}

size_t calc::ArrayDefVector::toLinear(std::vector<size_t>& ind) const
{
  std::map<std::vector<size_t>,size_t>::const_iterator 
    p = d_map2flatIndex.find(ind);
  POSTCOND(p != d_map2flatIndex.end());
  return p->second;
}

calc::ArrayDefVector::ArrayDefVector()
{
}

size_t calc::ArrayDefVector::size() const
{
  return d_vector.size();
}

const calc::ArrayDefinition *calc::ArrayDefVector::operator[](size_t i) const
{
  PRECOND(d_vector.size() > i);
  return d_vector[i];
}

bool calc::ArrayDefVector::operator==(const calc::ArrayDefVector& a) const
{
  return a.d_vector == d_vector;
}

std::string calc::ArrayDefVector::name() const
{
  std::string str = "";
  for(size_t i=0; i < d_vector.size(); i++)
      str += "["+d_vector[i]->name()+"]";
  return str;
}

size_t calc::ArrayDefVector::nrElements() const
{
  if (isArray())
    return d_indices.size();
  return 1;
}

const calc::ArrayDefVector::Index calc::ArrayDefVector::element(size_t n) const
{
  PRECOND(n < nrElements());
  Index index(d_vector.size());
  for(size_t i=0; i < d_vector.size(); i++) {
      index[i] = d_vector[i]->item(d_indices[n][i]);
  }
  return index;
}

std::string calc::ArrayDefVector::outputSuffix(size_t i)const
{
  if (!isArray())
    return "";
  std::string str= "";
  Index index = element(i);
  for(size_t a=0; a < d_vector.size(); a++) {
      PRECOND(a < index.size());
      str += "-"+index[a]->externalName();
  }
  return str;
}

bool calc::ArrayDefVector::isArray() const
{
  return size() > 0;
}

void calc::ArrayDefVector::print(calc::InfoScript& is)const
{
  for(size_t i=0; i < d_vector.size(); i++) {
    is.stream() << "[";
    is.parTag(d_vector[i]->name());
    is.stream() << "]";
  }
}
