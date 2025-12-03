#include "stddefx.h"
#include "calc_tssinputleaf.h"
#include "calc_tssinputparameter.h"
#include "calc_indexselected.h"
#include "calc_timetable.h"
#include "calc_usepar.h"
#include "calc_infoscript.h"
#include "calc_iscript.h"
#include "com_exception.h"

const calc::TimeTable *calc::TssInputLeaf::execute()
{
  return d_par->value(select());
}

size_t calc::TssInputLeaf::select() const
{
  return d_index->select();
}

void calc::TssInputLeaf::print(calc::InfoScript &i) const
{
  i.parTag(name());
}

//! ctor, modifies \a par for inputFilePath
/*!
    \todo  reverse of pcrcalc/test44: first input then output
 */
calc::TssInputLeaf::TssInputLeaf(UsePar &par, VS vsOfResult) : Symbol(par), d_index(par.createSelector())
{
  if (par.isArray()) {
    posError("Array of tss not yet implemented");
  }

  // first cast to more generic
  auto *p = dynamic_cast<TssParameter *>(script().findRightParameter(par, VS_TSS));
  d_par = dynamic_cast<TssInputParameter *>(p);
  if (d_par) {  // found
    return;
  }
  if (p) {  // it is a tss but not an input! pcrcalc/test44
    posError(p->userName() +
             "\n is already defined as an timeoutput"
             " on " +
             p->definitionPoint() +
             "\n"
             "can not mix timeinput and timeoutput\n");
  }
  // load value
  par.setInputFilePath();
  std::vector<TimeTable *> tss(1);
  try {
    expectedFileType(par.externalName(), VS_TSS);
    tss[0] = new TimeTable(par.externalName(), vsOfResult, scriptConst().nrTimeSteps());
  } catch (com::Exception &msg) {
    // pcrcalc/test45
    // pcrcalc/test226
    par.symError(msg);
  }
  d_par = new TssInputParameter(par, true, tss);
  script().addSymbol(d_par);
}

calc::TssInputLeaf::~TssInputLeaf()
{
}
