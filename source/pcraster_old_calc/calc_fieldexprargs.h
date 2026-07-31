#ifndef INCLUDED_OLDCALC_FIELDEXPRARGS
#define INCLUDED_OLDCALC_FIELDEXPRARGS

#include <vector>


namespace calc {

class FieldExpr;

using FieldExprArgs = std::vector<FieldExpr *>;
void cleanUp(FieldExprArgs&  argsVect);
void cleanUp(const FieldExprArgs&  argsVect);

}

#endif
