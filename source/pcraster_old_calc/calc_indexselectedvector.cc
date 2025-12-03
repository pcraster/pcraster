#include "stddefx.h"
#include "calc_indexselectedvector.h"
#include "calc_arraydefinition.h"
#include "calc_indexparameter.h"
#include "calc_indexparameterconstant.h"

#include <utility>

//! factor an ArrayDefVector
calc::ArrayDefVector calc::IndexSelectedVector::arrayDefVector() const
{
  std::vector<const calc::ArrayDefinition *> vector;
  for (size_t i = 0; i < size(); i++) {
    vector.push_back(d_vector[i]->partOf());
  }
  return calc::ArrayDefVector(vector);
}

calc::IndexSelectedVector::IndexSelectedVector(std::vector<const calc::IndexParameter *> vector)
    : d_vector(std::move(vector))
{
}

calc::IndexSelectedVector::~IndexSelectedVector()
{
}

int calc::IndexSelectedVector::select() const
{
  if (!size()) {
    return 0;
  }
  std::vector<size_t> ind(size());
  for (size_t i = 0; i < d_vector.size(); i++) {
    ind[i] = d_vector[i]->index();
  }
  return arrayDefVector().toLinear(ind);
}

//! name of current invocation
std::string calc::IndexSelectedVector::selectedName() const
{
  std::string str = "";
  std::vector<size_t> const ind(size());
  for (auto i : d_vector) {
    str += "[" + i->indexParameterConstant()->name() + "]";
  }
  return str;
}

//! name of current invocation
std::string calc::IndexSelectedVector::variableName() const
{
  std::string str = "";
  std::vector<size_t> const ind(size());
  for (auto i : d_vector) {
    str += "[" + i->name() + "]";
  }
  return str;
}

std::string calc::IndexSelectedVector::arrayDefName() const
{
  return arrayDefVector().name();
}

const calc::IndexParameter *calc::IndexSelectedVector::operator[](size_t n) const
{
  PRECOND(d_vector.size() > n);
  return d_vector[n];
}

bool calc::IndexSelectedVector::equal(const calc::IndexSelectedVector *index2) const
{
  PRECOND(size() == index2->size());
  for (size_t i = 0; i < d_vector.size(); i++) {
    if (d_vector[i] != (*index2)[i]) {
      return false;
    }
  }
  return true;
}
