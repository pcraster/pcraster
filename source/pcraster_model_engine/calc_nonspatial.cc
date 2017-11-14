#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_NONSPATIAL
#include "calc_nonspatial.h"
#define INCLUDED_CALC_NONSPATIAL
#endif

#ifndef INCLUDED_COM_CSFCELL
#include "com_csfcell.h"
#define INCLUDED_COM_CSFCELL
#endif

#ifndef INCLUDED_CALC_DOMAINERROR
#include "calc_domainerror.h"
#define INCLUDED_CALC_DOMAINERROR
#endif

//! constructor
/*! default to zero, if we do not init later,
    is now needed for the skipped exection of an if branch,
    because the impls. will test on not-MV of a nonspatial
 */
calc::NonSpatial::NonSpatial(VS vs, double value):
  Field(vs,CRI_X)
{
  switch(cr()) {
   case CR_REAL4: d_vals = (REAL4)value; break;
   case CR_INT4:  d_val4 = (INT4)value;  break;
   case CR_UINT1: d_val1 = (UINT1)value; break;
   default : POSTCOND(FALSE); // NEVER
  }
  POSTCOND(!isMV());
}

//! copy ctor
calc::NonSpatial::NonSpatial(const NonSpatial& rhs):
  Field(rhs.vs(),rhs.cri())
{
  switch(cr()) {
    case CR_REAL4:d_vals=rhs.d_vals; break;
    case CR_INT4: d_val4=rhs.d_val4; break;
    case CR_UINT1:d_val1=rhs.d_val1; break;
    default : POSTCOND(FALSE); // NEVER
  }
}

namespace calc {
//! initialization with a value
template<>
PCR_DLL_CLASS NonSpatial::NonSpatial(VS vs, const UINT1& value):
   Field(vs,crIndex<UINT1>())
{
   d_val1 = value;
}
//! initialization with a value
template<>
PCR_DLL_CLASS NonSpatial::NonSpatial(VS vs, const INT4& value):
   Field(vs,crIndex<INT4>())
{
   d_val4 = value;
}

//! initialization with a value
template<>
PCR_DLL_CLASS NonSpatial::NonSpatial(VS vs, const REAL4& value):
   Field(vs,crIndex<REAL4>())
{
   d_vals = value;
}

}

void calc::NonSpatial::analyzeBoolean(bool& noneAreTrue,bool& noneAreFalse) const
{
/*
  // pcrcalc379
  PRECOND(vs() == VS_B);
  PRECOND(cr() == CR_UINT1);
 */
  noneAreTrue = noneAreFalse = true;
  if (isMV())
    return;
  if ( ((int)getValue()) != 0 )
    noneAreTrue  = false;
  else
    noneAreFalse = false;
}

calc::NonSpatial::~NonSpatial()
{
}

size_t calc::NonSpatial::nrValues() const
{
  return 1;
}

//! \throws DomainError if \a value is MV
void calc::NonSpatial::setCell(const double& value, size_t /* i */)
{
  if (pcr::isMV(value))
    throw DomainError("NonSpatial set to MV");

  switch(cr()) {
   case CR_REAL4: d_vals = (REAL4)value; break;
   case CR_INT4:  d_val4 = (INT4)value; break;
   case CR_UINT1: d_val1 = (UINT1)value; break;
   default : POSTCOND(FALSE); // NEVER
  }
}

bool calc::NonSpatial::getCell(double& value, size_t /* i */) const
{
  value = getValue();
  return true;
}


const void *calc::NonSpatial::src() const
{
  PRECOND(!isMV()); // should have a value
  switch(cr()) {
    case CR_REAL4: return &d_vals;
    case CR_INT4:  return &d_val4;
    case CR_UINT1: return &d_val1;
    default : POSTCOND(FALSE); // NEVER
              return &d_vals;
  }
}

void *calc::NonSpatial::dest()
{
  switch(cr()) {
    case CR_REAL4: return &d_vals;
    case CR_INT4:  return &d_val4;
    case CR_UINT1: return &d_val1;
    default : POSTCOND(FALSE); // NEVER
              return &d_vals;
  }
}

const void *calc::NonSpatial::voidValue() const
{
  switch(cr()) {
    case CR_REAL4: return &d_vals;
    case CR_INT4:  return &d_val4;
    case CR_UINT1: return &d_val1;
    default : POSTCOND(FALSE); // NEVER
              return &d_vals;
  }
}

double calc::NonSpatial::getValue()const
{
  PRECOND(!isMV());
  switch(cr()) {
    case CR_REAL4: return (double)d_vals;
    case CR_INT4:  return (double)d_val4;
    case CR_UINT1: return (double)d_val1;
    default : POSTCOND(FALSE); // NEVER
               return (double)d_vals;
  }
}

bool calc::NonSpatial::isMV()const
{
  switch(cr()) {
    case CR_REAL4: return pcr::isMV(d_vals);
    case CR_INT4:  return pcr::isMV(d_val4);
    case CR_UINT1: return pcr::isMV(d_val1);
    default : POSTCOND(FALSE); // NEVER
              return false;
  }
}

bool calc::NonSpatial::isSpatial() const
{
  return false;
}

calc::NonSpatial* calc::NonSpatial::createClone() const
{
  return new NonSpatial(*this);
}
