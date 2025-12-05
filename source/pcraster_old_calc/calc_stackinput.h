#ifndef INCLUDED_OLDCALC_STACKINPUT
#define INCLUDED_OLDCALC_STACKINPUT

#include "calc_fieldexpr.h"
#include "calc_bindedsymbol.h"
#include "calc_fieldtype.h"

#include <vector>



namespace calc {

class StackReader;

//! input is a stack of maps: timeinput(stack)
/*!
    \todo
       derive a timeinputinterval(stack), that will
       fall back to some prev. timestep if current not
       present.
 */
class StackInput : public FieldExpr {
private:
  //! implements a format specific stack reader strategy
  const StackReader *d_reader{nullptr};
  // holds index for each timestep
  std::vector<size_t> d_itemToLoad;
  //! type of the stack
  FieldType d_type;


protected:
  void skipExecution() override;
  const FieldType &fieldType()const override;
public:
  //! posSymbol? should be Element
  StackInput(
    const Element&      posSymbol,
    const BindedSymbol& stackName,
          bool          sparse);

  ~StackInput() override;

  // MANIPULATORS
  void buildTypesRecursive(VS resultVsSet) override;

  FieldType& restrictType() override;

  void prepareExecution() override;
  void execute(FieldStack& stack) override;

  // ACCESSORS
  void print(InfoScript &si) const override;
};

}

#endif
