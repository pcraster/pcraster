#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_PARSET
#include "calc_parset.h"
#define INCLUDED_CALC_PARSET
#endif

// Library headers.
#ifndef INCLUDED_ALGORITHM
#include <algorithm>
#define INCLUDED_ALGORITHM
#endif
// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_CALC_ASTPAR
#include "calc_astpar.h"
#define INCLUDED_CALC_ASTPAR
#endif

#include <iterator>

//! find or return 0
/*!
 * hides std::set<>::find.
 * \returns element of ParSet that equals \a key or 0 if not
 *          found
 */
calc::ASTPar* calc::ParSet::find(ASTPar *key) const
{
  const_iterator pos= Base::find(key);
  if (pos != end())
    return *pos;
  return 0;
}

//! copy set \a s into vector in ASTParPtrLessName sort order.
std::vector<calc::ASTPar *> calc::ParSet::toSortedVector() const
{
  std::vector<calc::ASTPar *> v;
  std::copy(begin(),end(),std::back_inserter(v));
  return v;
}

//! convenience wrapper around std::set_union
calc::ParSet calc::setUnion(const ParSet& e1, const ParSet& e2)
{
  ParSet r;
  std::set_union(e1.begin(),e1.end(),
                 e2.begin(),e2.end(),
                 std::inserter(r,r.begin()),
                 ASTParPtrLessName());
  return r;
}

//! convenience wrapper around std::set_difference
calc::ParSet calc::setDifference(const ParSet& e1, const ParSet& e2)
{
  ParSet r;
  std::set_difference(e1.begin(),e1.end(),
                      e2.begin(),e2.end(),
                      std::inserter(r,r.begin()),
                      ASTParPtrLessName());
  return r;
}

//! convenience wrapper around std::set_intersection
calc::ParSet calc::setIntersection(const ParSet& e1, const ParSet& e2)
{
  ParSet r;
  std::set_intersection(e1.begin(),e1.end(),
                      e2.begin(),e2.end(),
                      std::inserter(r,r.begin()),
                      ASTParPtrLessName());
  return r;
}

bool calc::operator==(const calc::ParSet& e1,
                const std::set<std::string>& e2)
{
  std::set<std::string> namePars;
  for(calc::ParSet::const_iterator i=e1.begin(); i!=e1.end(); ++i)
    namePars.insert((*i)->name());
  return namePars==e2;
}



bool calc::operator==(const calc::ParSet& e1,
                      const calc::ParSet& e2)
{
  if (e1.size()!=e2.size())
    return false;
  ASTParPtrEqName c;
  return std::equal(e1.begin(),e1.end(),e2.begin(),c);
}

bool calc::operator!=(const calc::ParSet& e1,
                      const calc::ParSet& e2)
{
  return !(e1 == e2);
}



std::ostream& calc::operator<<(std::ostream& s,const calc::ParSet& p)
{
  s << "(";
  for(calc::ParSet::const_iterator i=p.begin(); i!=p.end(); ++i)
      s << (*i)->name() <<",";
  s << ")";
  return s;
}
