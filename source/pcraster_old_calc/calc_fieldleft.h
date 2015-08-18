#ifndef INCLUDED_CALC_FIELDLEFT
#define INCLUDED_CALC_FIELDLEFT

#ifndef INCLUDED_CALC_WRITEINFO
# include "calc_writeinfo.h"
#define INCLUDED_CALC_WRITEINFO
#endif

#ifndef INCLUDED_CALC_VS
# include "calc_vs.h"
#define INCLUDED_CALC_VS
#endif

#ifndef INCLUDED_CALC_SYMBOL
# include "calc_symbol.h"
#define INCLUDED_CALC_SYMBOL
#endif

#ifndef INCLUDED_CALC_USEDEFNODE
# include "calc_usedefnode.h"
#define INCLUDED_CALC_USEDEFNODE
#endif

#ifndef INCLUDED_CALC_FIELD
# include "calc_field.h"
#define INCLUDED_CALC_FIELD
#endif

namespace calc {

class UsePar;
class FieldNewParameter;
class FieldType;
class FieldExpr;
class InfoScript;
class IndexSelected;

//! left-side of expression (where values are assigned to parameters)
class FieldLeft : public Symbol, public UseDefNode {
  WriteInfo          d_write;
  IndexSelected     *d_index;
  FieldNewParameter *d_par;

   void cleanUp();
 protected:
  void analyseUseDef();
  const FieldType& fieldType();
 public:
  // CREATORS
  FieldLeft(
      StatementBlock *b,
      const WriteInfo& write,
      const UsePar &field,VS vsRight);
  virtual ~FieldLeft();
  // MANIPULATORS
  // restrict FieldType based on assigned right expression
  bool restrictUser(const FieldExpr *right);
  bool restrictUser(const FieldType& right);

  void prepareExecution();

  void assign(FieldHandle e);

  // ACCESSORS
  void print(InfoScript& i)const;
  bool spatial() const;
  VS vs() const;
  bool isUse() const;

  IndexSelected *indexSelected() const { return d_index; };
};

}

#endif
