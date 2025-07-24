#include "stddefx.h"
#include "calc_fieldleaf.h"
#include "calc_fieldleft.h"
#include "calc_fieldmapinputparameter.h"
#include "calc_indexselectedvector.h"
#include "calc_infoscript.h"
#include "calc_iscript.h"
#include "calc_fieldstack.h"
#include "calc_field.h"
#include "calc_usepar.h"

// int  repeatCondition=0;

//! ctor, can modify \a par
calc::FieldLeaf::FieldLeaf(
    UsePar &par):
  FieldExpr(par), UseDefNode(*(par.block())),
  d_index(par.createSelector()),d_overWriteVal(false)
{
  d_par = dynamic_cast<FieldParameter *>(
    script().findRightParameter(par,VS_FIELD));
  if (!d_par) {
    par.setInputFilePath();
    d_par = script().addExternalFieldLeaf(par);
  }
}

bool calc::FieldLeaf::isFieldLeaf() const
{
  return true;
}

bool calc::FieldLeaf::isUse() const
{
  return true;
}

void calc::FieldLeaf::buildTypes()
{
  // nothing to do a field has a fixed type
}

void calc::FieldLeaf::buildTypesRecursive(VS resultVsSet)
{
  if (isSubset(resultVsSet,vs())) // expr is polymorphic
    restrictType().restrictSystem(resultVsSet,spatial());
}

void calc::FieldLeaf::prepareExecution()
{
  d_par->addToChain(this);
}

/*!
 * \todo the algo below is not very complete
 *          or well thought
 * \todo
 *      am i doing variable renaming?
 * \todo
 *      am i doing dead code removal with user warnings?
 */
void calc::FieldLeaf::analyseUseDef()
{
  // I am a use node
  /* note that d_overWriteVal over a single element
   *  of an array, while deleteAtEndOfBlock removes
   *  all elements
   */

  /* if next is not a use, then I may be
   * deleted, if not bail out
   */
   if (!nextIsNotUse())
    return;
   // So the next is not a use:
   // on the (possible) next there is no need
   // for the current value: the next is a
   // (re-)definition or there is no next
   if (!d_par->isArray()) {
       if (nextInSameBlock() // there is a next definition
            || (! deleteValueAtEndOfBlock(d_par,false))
          )
          d_overWriteVal = true;
  }
  else {
    const calc::UseDefNode *next = nextUseDef();
    if (   (!next) ||                  // there is no next
           (next == d_par->firstDef()) // of the next is the first def
        )  {
         if (! deleteValueAtEndOfBlock(d_par,true))
            d_overWriteVal = true;
         return;
       }
    // there is a next, and is a def
    // cast to fieldleft to optimize a[i] = a[i] + f(......)
    const auto *nextDef =
     dynamic_cast<const calc::FieldLeft *>(next);
    const auto *indexNextDef =
     dynamic_cast<const calc::IndexSelectedVector *>(nextDef->indexSelected());
    const auto *index =
    dynamic_cast<const calc::IndexSelectedVector *>(d_index.get());
    if (index->equal(indexNextDef))
            d_overWriteVal = true;
  }
}

void calc::FieldLeaf::skipExecution()
{
  try {
  // if we skip, do get the value, so it is able to clean up
  //  if d_overWriteVal is true
  calc::FieldHandle f = d_par->value(d_index->select(), d_overWriteVal);
  }
  catch(const calc::Field::NotInitialized&) {
    // not initialized, forget about it, no deletion neccessary
  }
}
void calc::FieldLeaf::execute(calc::FieldStack& stack)
{
  try {
    stack.push(d_par->value(d_index->select(), d_overWriteVal));
  } catch(const calc::Field::NotInitialized&) {
    runtimeError(
     d_par->userName()+d_index->selectedName()+" not initialized");
  }
}

calc::FieldType& calc::FieldLeaf::restrictType()
{
  return d_par->restrictType();
}

const calc::FieldType &calc::FieldLeaf::fieldType() const
{
  return d_par->fieldType();
}

void calc::FieldLeaf::print(calc::InfoScript& i)const
{
  if (d_overWriteVal)
    i.stream() << "<I>";
  i.parTag(d_par->name());
  i.stream() << d_index->variableName();
  if (d_overWriteVal)
    i.stream() << "</I>";
}

