#ifndef INCLUDED_CALC_MIXEDEXPR
#define INCLUDED_CALC_MIXEDEXPR

#ifndef INCLUDED_CALC_FIELDEXPR
# include "calc_fieldexpr.h"
#define INCLUDED_CALC_FIELDEXPR
#endif

#ifndef INCLUDED_CALC_FIELDARGS
# include "calc_fieldargs.h"
#define INCLUDED_CALC_FIELDARGS
#endif

#ifndef INCLUDED_FIELDEXPRARGS
# include "calc_fieldexprargs.h"
#define INCLUDED_FIELDEXPRARGS
#endif

#ifndef INCLUDED_CALC_FIELDTYPE
# include "calc_fieldtype.h"
#define INCLUDED_CALC_FIELDTYPE
#endif

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
  void skipExecution();
  const FieldType& fieldType() const;
 public:
  // CREATORS
  MixedExpr(const Element& pos,
      const Operator& op,
            FieldExprArgs& fieldArgs);
  virtual ~MixedExpr() {};

  // MANIPULATORS
  void prepareExecution();
  void buildTypesRecursive(VS resultVsSet);
  FieldType& restrictType();
  // ACCESSORS
  void printFieldArgs(InfoScript& si)const;

};

}

#endif
