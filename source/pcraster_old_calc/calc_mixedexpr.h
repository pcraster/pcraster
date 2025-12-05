#ifndef INCLUDED_OLDCALC_MIXEDEXPR
#define INCLUDED_OLDCALC_MIXEDEXPR

#include "calc_fieldexpr.h"
#include "calc_fieldargs.h"
#include "calc_fieldexprargs.h"
#include "calc_fieldtype.h"


namespace calc {

class Operator;
class FieldStack;
class InfoScript;

//! expression having a non-field node
/* the first node is something else
 */
class MixedExpr : public FieldExpr, public FieldArgs {
 private:
   FieldType        d_type;
 protected:
  void buildTypes();
  void skipExecution() override;
  const FieldType& fieldType() const override;
 public:
  // CREATORS
  MixedExpr(const Element& pos,
      const Operator& op,
            FieldExprArgs& fieldArgs);
  ~MixedExpr() override {}

  // MANIPULATORS
  void prepareExecution() override;
  void buildTypesRecursive(VS resultVsSet) override;
  FieldType& restrictType() override;
  // ACCESSORS
  void printFieldArgs(InfoScript& si)const;

};

}

#endif
