#ifndef INCLUDED_OLDCALC_BRANCHEXPR
#define INCLUDED_OLDCALC_BRANCHEXPR

#include "calc_fieldexpr.h"
#include "calc_fieldargs.h"
#include "calc_fieldtype.h"
#include "calc_fieldexprargs.h"


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
  void skipExecution() override;
  const FieldType& fieldType() const override;

  // CREATORS
  BranchExpr(
    const Element&       pos,
    const Operator&      op,
          FieldExprArgs& fieldArgs);
public:
  // MANIPULATORS
  void prepareExecution() override;
  void buildTypesRecursive(VS resultVsSet) override;
  FieldType& restrictType() override;
  // ACCESSORS
  size_t nrArgs() const;
  void print(InfoScript& i)const override;
};

}

#endif
