#ifndef INCLUDED_CALC_CONSTANT
#define INCLUDED_CALC_CONSTANT

#ifndef INCLUDED_CALC_FIELDEXPR
# include "calc_fieldexpr.h"
#define INCLUDED_CALC_FIELDEXPR
#endif

#ifndef INCLUDED_CALC_FIELDTYPE
# include "calc_fieldtype.h"
#define INCLUDED_FIELDYPE
#endif

namespace calc {

class FieldStack;
class Symbol;

//! an expression leaf holding a constant number
class Constant : public FieldExpr {
 private:
  //! the value as string (exactly as parsed!)
  const   std::string d_strRepr;
   double  d_value;
   FieldType d_type;
  // NOTE CW name() is defined to const
  // later we can pass the exact value there!
  void buildTypes();
  void skipExecution();
protected:
  const FieldType& fieldType() const { return d_type;};
 public:
  // CREATORS
  Constant(const Symbol& name);
  Constant(
    const Symbol& castFunctionName,
    VS            castDestination,
    const Symbol &v);

  // MANIPULATORS
  FieldType& restrictType();
  void buildTypesRecursive(VS resultVsSet);
  void prepareExecution();
  void execute(FieldStack& s);
  // ACCESSORS
  void print(InfoScript& i)const;
  double value() const { return d_value; };
  VS vs() const { return d_type.vs(); };
  bool isConstant() const;

  std::string strRepr() const { return d_strRepr; };
  std::string qName() const;
};

}

#endif
