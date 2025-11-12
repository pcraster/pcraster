#ifndef INCLUDED_CALC_FIELDEXPRARGS
#define INCLUDED_CALC_FIELDEXPRARGS

#include <vector>


namespace calc {

class FieldExpr;

typedef std::vector<FieldExpr *>FieldExprArgs;
void cleanUp(FieldExprArgs&  argsVect);
void cleanUp(const FieldExprArgs&  argsVect);

}

#endif
