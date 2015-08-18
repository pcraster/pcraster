#include "stddefx.h"

#ifndef INCLUDED_CALC_FIELDLEFT
#include "calc_fieldleft.h"
#define INCLUDED_CALC_FIELDLEFT
#endif

#ifndef INCLUDED_SSTREAM
#include <sstream>
#define INCLUDED_SSTREAM
#endif

#ifndef INCLUDED_CALC_FIELDEXPR
#include "calc_fieldexpr.h"
#define INCLUDED_CALC_FIELDEXPR
#endif

#ifndef INCLUDED_CALC_FIELDNEWPARAMETER
#include "calc_fieldnewparameter.h"
#define INCLUDED_CALC_FIELDNEWPARAMETER
#endif

#ifndef INCLUDED_CALC_ISCRIPT
#include "calc_iscript.h"
#define INCLUDED_CALC_ISCRIPT
#endif

#ifndef INCLUDED_CALC_USEPAR
#include "calc_usepar.h"
#define INCLUDED_CALC_USEPAR
#endif

#ifndef INCLUDED_CALC_INDEXSELECTED
#include "calc_indexselected.h"
#define INCLUDED_CALC_INDEXSELECTED
#endif

#ifndef INCLUDED_CALC_INFOSCRIPT
#include "calc_infoscript.h"
#define INCLUDED_CALC_INFOSCRIPT
#endif

#ifndef INCLUDED_CALC_SPATIAL
#include "calc_spatial.h"
#define INCLUDED_CALC_SPATIAL
#endif
#ifndef INCLUDED_CALC_ZEROMAP
#include "calc_zeromap.h"
#define INCLUDED_CALC_ZEROMAP
#endif

calc::FieldLeft::FieldLeft(
            StatementBlock *b,
      const WriteInfo& write,
      const UsePar &field,
      VS    vsRight) :
  Symbol(field),UseDefNode(*b),
  d_write(write),d_index(field.createSelector())
{
  try {
   FieldParameter *p = dynamic_cast<FieldParameter *>
    (script().findLeftParameter(field,vsRight));
  if (!p) {
    d_par = new FieldNewParameter(field,false, false, VS_FIELD,ST_DERIVED);
    script().addSymbol(d_par);
  } else {
    if (p->isConstantBinding())  // pcrcalc/test0
      posError("Assigning value to constant binding: "+quote(p->userName()));
    d_par = dynamic_cast<FieldNewParameter *>(p);
    POSTCOND(d_par);
  }
  d_par->setReportPoint(this->position(),d_write);
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
#ifndef INCLUDED_CALC_ZIPMAP
#include "calc_zipmap.h"
#define INCLUDED_CALC_ZIPMAP
#endif
#ifndef INCLUDED_IOSTREAM
#include <iostream>
#define INCLUDED_IOSTREAM
#endif


void calc::FieldLeft::assign(FieldHandle f)
{
  bool allZero(false);
  const Spatial *s(0);
  if (f->isSpatial() &&
       (scriptConst().debugMvAssignments()
      || scriptConst().zeroCompression())) {
     s=dynamic_cast<const Spatial *>(f.get_rep());
     POSTCOND(s);
     size_t bpc;
     if (s->checkDebug(scriptConst(),allZero,bpc)) {
        std::ostringstream msg;
        msg << "-d catched MV creation on " << name() << d_index->selectedName()
            << "\n  inspection map written to" << quote(scriptConst().debugMapName());
        runtimeError(msg.str());
     }
   }

 if (allZero && scriptConst().zeroCompression())
   d_par->assign(FieldHandle(new ZeroMap(s)),d_index->select(),this->position());
 else 
    d_par->assign(f,d_index->select(),this->position());
}

const calc::FieldType& calc::FieldLeft::fieldType()
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

bool calc::FieldLeft::restrictUser(const calc::FieldType& right)
{
  try {
    return d_par->restrictUser(right);
  }
  catch(const  calc::FieldParameter::RestrictError& r) {
    posError(r.what());
  }
  PRECOND(FALSE); // never reached
  return false;
}

bool calc::FieldLeft::restrictUser(const calc::FieldExpr* right)
{
  return restrictUser(right->fieldType());
}

void calc::FieldLeft::print(calc::InfoScript& i)const
{
  i.parTag(name());
  i.stream() << d_index->variableName();
}
