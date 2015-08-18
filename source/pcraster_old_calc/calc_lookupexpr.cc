#include "stddefx.h"

#ifndef INCLUDED_CALC_OPERATIONTIMER
#include "calc_operationtimer.h"
#define INCLUDED_CALC_OPERATIONTIMER
#endif

#ifndef INCLUDED_CALC_LOOKUPEXPR
# include "calc_lookupexpr.h"
#define INCLUDED_CALC_LOOKUPEXPR
#endif

#ifndef INCLUDED_CALC_LOOKUPTABLELEAF
# include "calc_lookuptableleaf.h"
#define INCLUDED_CALC_LOOKUPTABLELEAF
#endif

#ifndef INCLUDED_CALC_LOOKUPTABLE
# include "calc_lookuptable.h"
#define INCLUDED_CALC_LOOKUPTABLE
#endif

#ifndef INCLUDED_CALC_FIELDSTACK
# include "calc_fieldstack.h"
#define INCLUDED_CALC_FIELDSTACK
#endif

#ifndef INCLUDED_CALC_INFOSCRIPT
#include "calc_infoscript.h"
#define INCLUDED_CALC_INFOSCRIPT
#endif

#ifndef INCLUDED_COM_CSFCELL
#include "com_csfcell.h"
#define INCLUDED_COM_CSFCELL
#endif

#ifndef INCLUDED_CALC_FIELD
#include "calc_field.h"
#define INCLUDED_CALC_FIELD
#endif

#ifndef INCLUDED_CALC_OPERATOR
#include "calc_operator.h"  // check on OP_LINEARLOOKUP
#define INCLUDED_CALC_OPERATOR
#endif

calc::LookupTableLeaf *calc::LookupExpr::init(
  const calc::UsePar &tab)
{
  Args& fa = fieldArgs();
  std::vector<VS> readKeys(nrFieldArgs());
  for(size_t i=0; i < nrFieldArgs(); i++)
    readKeys[i] = fa[i]->vs();
  return new LookupTableLeaf(tab,vs(),readKeys,op().opCode()==OP_LOOKUPLINEAR);
}

//! create lookup... expr
calc::LookupExpr::LookupExpr(
  const Element& pos,
  const Operator& op,
  const UsePar &tab,
        FieldExprArgs& keyArgs):
        MixedExpr(pos, op, keyArgs),d_tab(init(tab))
{
  try {
    buildTypes();
  } catch (...) {
    cleanUp();
  }
}

void calc::LookupExpr::cleanUp()
{
  delete d_tab;
}

calc::LookupExpr::~LookupExpr()
{
  cleanUp();
}

void calc::LookupExpr::print(InfoScript& si)const
{
  fieldType().print(si,op());
  si.stream() << "(";
  d_tab->print(si);
  printFieldArgs(si);
  si.stream() << ")";
}

/*!
    \todo the call runtimeError(v.what()) in all code modules
          generates on bad_alloc, a cryptic message, change
          to "Not enough memory"

   \todo
    further optimization by checking if LookupTable is fully
    ordered: sort on <, then check in reverse if each sibling
    pair will hold on > comparision. Maybe remove last [,] key
    first before doing that check. If that test hold bsearch can
    be done. DAT MOET al in de constructie van LookupTable kunnen!
    En dus niet bij elke execute.

  \todo
    deze "optimalisatie" introduceerde de dynamic_cast hack,
    voor de LookupLinear
    moet er weer netjes uit.

   \todo
    if filter remains to keep 1 single value then assign that
    with no lookup.
    make test where filter removes everything!
 */
#ifndef INCLUDED_CALC_LOOKUPLINEAR
#include "calc_lookuplinear.h" // HACK HACK
#define INCLUDED_CALC_LOOKUPLINEAR
#endif
void calc::LookupExpr::execute(FieldStack& resStack)
{
  OPERATION_TIMER("lookup....",spatial());
  FieldStack stack;
  executeArgs(stack);
  FieldsPopped fields(stack,nrFieldArgs());

  FieldHandle result = createResultField();
  const LookupTable *tab = d_tab->execute();

  size_t              nrKeys(nrFieldArgs());
  std::vector<double> keyValues(nrKeys);
  std::vector<bool>   remove(nrKeys);
  std::vector<size_t> keep;

  size_t nr(result->nrValues());

  bool applyFilter=false;
  for(size_t k = 0; k < nrKeys; k++) {
    remove[k]=!fields[k]->isSpatial();
    if (remove[k]) {
      applyFilter=true;
      // non-spatial always defined
      PRECOND(fields[k]->getCell(keyValues[k],0));
      // set the key to position k
      fields[k]->getCell(keyValues[k],0);
    } else {
      keep.push_back(k);
    }
  }
  applyFilter=applyFilter &&
              (dynamic_cast<const LookupLinear*>(tab) ==0);

  try {
    if (applyFilter) {
      // filter out non-spatial keys to other table
      // and use that table
      LookupTable ftab(*tab,remove,keyValues);
      keyValues.resize(keep.size());
      for(size_t i=0; i < nr; i++) {
        double res;
        pcr::setMV(res); // if goto is exec'ed then ok value
        for(size_t k = 0; k < keep.size(); k++) {
          if (!fields[keep[k]]->getCell(keyValues[k], i))
            goto store1; // MV in input, MV in output
        }
        ftab.find(res, keyValues);
        store1:
          result->setCell(res, i);
      }

    } else {
      for(size_t i=0; i < nr; i++) {
        double res;
        pcr::setMV(res); // if goto is exec'ed then ok value
        for(size_t k = 0; k < nrKeys; k++) {
          if (!fields[k]->getCell(keyValues[k], i))
            goto store; // MV in input, MV in output
        }
        tab->find(res, keyValues);
        store:
          result->setCell(res, i);
      }
    }
  } catch(const Field::SetNonSpatialToMV& s) {
     // pcrcalc/test225
     Element::runtimeError("No match in lookup...");
  }
  resStack.push(result);
}
