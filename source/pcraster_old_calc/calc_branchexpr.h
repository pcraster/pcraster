#ifndef INCLUDED_CALC_BRANCHEXPR
#define INCLUDED_CALC_BRANCHEXPR

#ifndef INCLUDED_CALC_FIELDEXPR
# include "calc_fieldexpr.h"
#define INCLUDED_CALC_FIELDEXPR
#endif

#ifndef INCLUDED_CALC_FIELDARGS
# include "calc_fieldargs.h"
#define INCLUDED_CALC_FIELDARGS
#endif

#ifndef INCLUDED_CALC_FIELDTYPE
# include "calc_fieldtype.h"
#define INCLUDED_CALC_FIELDTYPE
#endif

#ifndef INCLUDED_CALC_FIELDEXPRARGS
# include "calc_fieldexprargs.h"
#define INCLUDED_CALC_FIELDEXPRARGS
#endif

namespace calc {

class Operator;

//! expression with operands of a function
/*  Describes a branch that holds a function or operator. Note that
 *  a function can have 0 arguments, thus holding no Leafs!
 */
class BranchExpr : public FieldArgs,public FieldExpr {
private:
  FieldType d_type;
  void special();
  void buildTypes();
  void defaultBuildType(VS& newVs, bool& isSpatial);
  void argCombError(int nr,VS  prevVs) const;
protected:
  void skipExecution();
  const FieldType& fieldType() const;

  // CREATORS
  BranchExpr(
    const Element&       pos,
    const Operator&      op,
          FieldExprArgs& fieldArgs);
public:
  // MANIPULATORS
  void prepareExecution();
  void buildTypesRecursive(VS resultVsSet);
  FieldType& restrictType();
  // ACCESSORS
  size_t nrArgs() const;
  void print(InfoScript& i)const;
};

}

#endif
