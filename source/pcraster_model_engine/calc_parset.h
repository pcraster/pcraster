#ifndef INCLUDED_CALC_PARSET
#define INCLUDED_CALC_PARSET

#include "stddefx.h"
#include "calc_astpar.h"

#include <iostream>
#include <set>
#include <vector>


namespace calc {
  // ParSet declarations.
}



namespace calc {

class ParSet : public std::set<ASTPar *,ASTParPtrLessName>
{
  typedef std::set<ASTPar *,ASTParPtrLessName> Base;
public:
  ASTPar*               find           (ASTPar*) const;
  std::vector<ASTPar *> toSortedVector ()        const;
};

//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------

ParSet setUnion        (const ParSet& e1, const ParSet& e2);
ParSet setDifference   (const ParSet& e1, const ParSet& e2);
ParSet setIntersection (const ParSet& e1, const ParSet& e2);


std::ostream &operator<<(std::ostream& s,const calc::ParSet& p);
bool operator==(const calc::ParSet& e1,
                const std::set<std::string>& e2);


bool operator==(const calc::ParSet& e1,
                const calc::ParSet& e2);

bool operator!=(const calc::ParSet& e1,
                const calc::ParSet& e2);



} // namespace calc


#endif
