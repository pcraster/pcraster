#ifndef INCLUDED_CALC_FIELDLEAF
#define INCLUDED_CALC_FIELDLEAF

#ifndef INCLUDED_CALC_FIELDEXPR
# include "calc_fieldexpr.h"
#define INCLUDED_CALC_FIELDEXPR
#endif

#ifndef INCLUDED_CALC_USEDEFNODE
# include "calc_usedefnode.h"
#define INCLUDED_CALC_USEDEFNODE
#endif

#ifndef INCLUDED_MEMORY
# include <memory>
#define INCLUDED_MEMORY
#endif

namespace calc {

class FieldParameter;
class FieldStack;
class UsePar;
class IndexSelected;

//! a left expression leaf holding a field parameter
class FieldLeaf : public FieldExpr, public UseDefNode {
 private:
  std::unique_ptr<IndexSelected> d_index;
  FieldParameter* d_par;
  void buildTypes();
  //! can value be overwritten at execution time, determined by usedef algorithm
  bool d_overWriteVal{false};
 protected:
  void analyseUseDef() override;
  void skipExecution() override;
  const FieldType &fieldType() const override;
 public:
  // CREATORS
  FieldLeaf(UsePar &field);
  // MANIPULATORS
  void buildTypesRecursive(VS resultVsSet) override;

  void prepareExecution() override;

   //execution phase
  void execute(FieldStack& s) override;
  FieldType& restrictType() override;
  // ACCESSORS
  void print(InfoScript& i)const override;
  bool isFieldLeaf() const override;
  bool isUse() const override;
};

}

#endif
