#include "stddefx.h"
#include "calc_parset.h"
#include "calc_astpar.h"

#include <algorithm>
#include <iterator>

//! find or return 0
/*!
 * hides std::set<>::find.
 * \returns element of ParSet that equals \a key or 0 if not
 *          found
 */
calc::ASTPar *calc::ParSet::find(ASTPar *key) const
{
  auto pos = Base::find(key);
  if (pos != end()) {
    return *pos;
  }
  return nullptr;
}

//! copy set \a s into vector in ASTParPtrLessName sort order.
std::vector<calc::ASTPar *> calc::ParSet::toSortedVector() const
{
  std::vector<calc::ASTPar *> v;
  std::copy(begin(), end(), std::back_inserter(v));
  return v;
}

//! convenience wrapper around std::set_union
calc::ParSet calc::setUnion(const ParSet &e1, const ParSet &e2)
{
  ParSet r;
  std::set_union(e1.begin(), e1.end(), e2.begin(), e2.end(), std::inserter(r, r.begin()),
                 ASTParPtrLessName());
  return r;
}

//! convenience wrapper around std::set_difference
calc::ParSet calc::setDifference(const ParSet &e1, const ParSet &e2)
{
  ParSet r;
  std::set_difference(e1.begin(), e1.end(), e2.begin(), e2.end(), std::inserter(r, r.begin()),
                      ASTParPtrLessName());
  return r;
}

//! convenience wrapper around std::set_intersection
calc::ParSet calc::setIntersection(const ParSet &e1, const ParSet &e2)
{
  ParSet r;
  std::set_intersection(e1.begin(), e1.end(), e2.begin(), e2.end(), std::inserter(r, r.begin()),
                        ASTParPtrLessName());
  return r;
}

bool calc::operator==(const calc::ParSet &e1, const std::set<std::string> &e2)
{
  std::set<std::string> namePars;
  for (auto i : e1) {
    namePars.insert(i->name());
  }
  return namePars == e2;
}

bool calc::operator==(const calc::ParSet &e1, const calc::ParSet &e2)
{
  if (e1.size() != e2.size()) {
    return false;
  }
  ASTParPtrEqName const c;
  return std::equal(e1.begin(), e1.end(), e2.begin(), c);
}

bool calc::operator!=(const calc::ParSet &e1, const calc::ParSet &e2)
{
  return !(e1 == e2);
}

std::ostream &calc::operator<<(std::ostream &s, const calc::ParSet &p)
{
  s << "(";
  for (auto i : p) {
    s << i->name() << ",";
  }
  s << ")";
  return s;
}
