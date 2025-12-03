#include "stddefx.h"
#include "calc_fieldleft.h"
#include "calc_fieldexpr.h"
#include "calc_fieldnewparameter.h"
#include "calc_iscript.h"
#include "calc_usepar.h"
#include "calc_indexselected.h"
#include "calc_infoscript.h"
#include "calc_spatial.h"
#include "calc_zeromap.h"
#include <sstream>

calc::FieldLeft::FieldLeft(StatementBlock *b, const WriteInfo &write, const UsePar &field, VS vsRight)
    : Symbol(field), UseDefNode(*b), d_write(write), d_index(field.createSelector())
{
  try {
    auto *p = dynamic_cast<FieldParameter *>(script().findLeftParameter(field, vsRight));
    if (!p) {
      d_par = new FieldNewParameter(field, false, false, VS_FIELD, ST_DERIVED);
      script().addSymbol(d_par);
    } else {
      if (p->isConstantBinding()) {  // pcrcalc/test0
        posError("Assigning value to constant binding: " + quote(p->userName()));
      }
      d_par = dynamic_cast<FieldNewParameter *>(p);
      POSTCOND(d_par);
    }
    d_par->setReportPoint(this->position(), d_write);
  } catch (...) {
    cleanUp();
    throw;
  }
}

calc::FieldLeft::~FieldLeft()
{
  cleanUp();
}

void calc::FieldLeft::cleanUp()
{
  delete d_index;
}

#include "calc_zipmap.h"
#include <iostream>

void calc::FieldLeft::assign(FieldHandle f)
{
  bool allZero(false);
  const Spatial *s(nullptr);
  if (f->isSpatial() && (scriptConst().debugMvAssignments() || scriptConst().zeroCompression())) {
    s = dynamic_cast<const Spatial *>(f.get_rep());
    POSTCOND(s);
    size_t bpc = 0;
    if (s->checkDebug(scriptConst(), allZero, bpc)) {
      std::ostringstream msg;
      msg << "-d catched MV creation on " << name() << d_index->selectedName()
          << "\n  inspection map written to" << quote(scriptConst().debugMapName());
      runtimeError(msg.str());
    }
  }

  if (allZero && scriptConst().zeroCompression()) {
    d_par->assign(FieldHandle(new ZeroMap(s)), d_index->select(), this->position());
  } else {
    d_par->assign(f, d_index->select(), this->position());
  }
}

const calc::FieldType &calc::FieldLeft::fieldType()
{
  return d_par->fieldType();
}

bool calc::FieldLeft::spatial() const
{
  return d_par->fieldType().spatial();
}

void calc::FieldLeft::prepareExecution()
{
  d_par->addToChain(this);
}

bool calc::FieldLeft::isUse() const
{
  return false;
}

void calc::FieldLeft::analyseUseDef()
{
}

VS calc::FieldLeft::vs() const
{
  return d_par->fieldType().vs();
}

bool calc::FieldLeft::restrictUser(const calc::FieldType &right)
{
  try {
    return d_par->restrictUser(right);
  } catch (const calc::FieldParameter::RestrictError &r) {
    posError(r.what());
  }
  PRECOND(false);  // never reached
  return false;
}

bool calc::FieldLeft::restrictUser(const calc::FieldExpr *right)
{
  return restrictUser(right->fieldType());
}

void calc::FieldLeft::print(calc::InfoScript &i) const
{
  i.parTag(name());
  i.stream() << d_index->variableName();
}
