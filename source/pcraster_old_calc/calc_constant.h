#ifndef INCLUDED_OLDCALC_CONSTANT
#define INCLUDED_OLDCALC_CONSTANT

#include "calc_fieldexpr.h"
#include "calc_fieldtype.h"


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
  void skipExecution() override;
protected:
  const FieldType& fieldType() const override { return d_type;}
 public:
  // CREATORS
  Constant(const Symbol& name);
  Constant(
    const Symbol& castFunctionName,
    VS            castDestination,
    const Symbol &v);

  // MANIPULATORS
  FieldType& restrictType() override;
  void buildTypesRecursive(VS resultVsSet) override;
  void prepareExecution() override;
  void execute(FieldStack& s) override;
  // ACCESSORS
  void print(InfoScript& i)const override;
  double value() const { return d_value; }
  VS vs() const { return d_type.vs(); }
  bool isConstant() const override;

  std::string strRepr() const { return d_strRepr; }
  std::string qName() const;
};

}

#endif
