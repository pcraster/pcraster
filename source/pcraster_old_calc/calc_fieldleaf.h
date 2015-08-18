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
  std::auto_ptr<IndexSelected> d_index;
  FieldParameter* d_par;
  void buildTypes();
  //! can value be overwritten at execution time, determined by usedef algorithm
  bool d_overWriteVal;
 protected:
  void analyseUseDef();
  void skipExecution();
  const FieldType &fieldType() const;
 public:
  // CREATORS
  FieldLeaf(UsePar &field);
  // MANIPULATORS
  void buildTypesRecursive(VS resultVsSet);

  void prepareExecution();

   //execution phase
  void execute(FieldStack& s);
  FieldType& restrictType();
  // ACCESSORS
  void print(InfoScript& i)const;
  bool isFieldLeaf() const;
  bool isUse() const;
};

}

#endif
