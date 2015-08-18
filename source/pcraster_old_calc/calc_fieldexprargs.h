#ifndef INCLUDED_CALC_FIELDEXPRARGS
#define INCLUDED_CALC_FIELDEXPRARGS

#ifndef INCLUDED_VECTOR
# include <vector>
#define INCLUDED_VECTOR
#endif

namespace calc {

class FieldExpr;

typedef std::vector<FieldExpr *>FieldExprArgs;
void cleanUp(FieldExprArgs&  argsVect);
void cleanUp(const FieldExprArgs&  argsVect);

}

#endif
