#include "stddefx.h"

#ifndef INCLUDED_CSTRING
#include <cstring>
#define INCLUDED_CSTRING
#endif

#ifndef INCLUDED_CALC_FIELD
#include "calc_field.h"
#define INCLUDED_CALC_FIELD
#endif

#ifndef INCLUDED_CALC_DATATYPE
#include "calc_datatype.h"
#define INCLUDED_CALC_DATATYPE
#endif


calc::Field::Field(const Field& rhs):
  DataValue(rhs),
  d_vs(rhs.d_vs),
  d_cri(rhs.d_cri)
{
}

calc::Field::Field(VS vs, CRIndex cri):
  d_vs(vs),
  d_cri(cri)
{
   if (d_cri == CRI_X)
    d_cri=allFitCRIndex(d_vs);
}

calc::Field::~Field()
{
}

calc::Field* calc::Field::load()
{
  return this;
}

UINT1 *calc::Field::dest_1 () {
  PRECOND(cri() == CRI_1);
  return static_cast<UINT1 *>(dest());
}
REAL4 *calc::Field::dest_f () {
  PRECOND(cri() == CRI_f);
  return static_cast<REAL4 *>(dest());
}
INT4  *calc::Field::dest_4 () {
  PRECOND(cri() == CRI_4);
  return static_cast<INT4 *>(dest());
}

//! fill Field value from \a src using std::memcpy
void calc::Field::beMemCpyDest(const void *src)
{
  std::memcpy(dest(),src,nrValues()*size(cri()));
}

//! copy Field value to \a dest using std::memcpy
void calc::Field::beMemCpySrc(void *dest) const
{
  std::memcpy(dest,src(),nrValues()*size(cri()));
}

const UINT1 *calc::Field::src_1 () const {
  PRECOND(cri() == CRI_1);
  return static_cast<const UINT1 *>(src());
}
const REAL4 *calc::Field::src_f () const {
  PRECOND(cri() == CRI_f);
  return static_cast<const REAL4 *>(src());
}
const INT4  *calc::Field::src_4 () const {
  PRECOND(cri() == CRI_4);
  return static_cast<const INT4 *>(src());
}


VS calc::Field::vs()const
{
  return d_vs;
}

calc::OVS calc::Field:: ovs() const
{
  return vs();
}

CSF_CR calc::Field::cr()const
{
  return calc::cr(d_cri);
}
calc::CRIndex calc::Field::cri()const
{
  return d_cri;
}

calc::DataType calc::Field::type() const
{
  return DataType(vs(),isSpatial());
}

//! see only implementation: Spatial::findMVinMask
calc::Field* calc::Field::findMVinMask(const std::vector<bool>& /* areaMask */) const
{
  return 0;
}

/*! this is needed if one reuses a field, for example
 * sin accept VS_D and returns VS_S, see calc::Trig, calc::Conversion
 * ExecArguments::dest(size_t a) seems to be only use
 * and a "hack" in RunTimeEnv
 */
void calc::Field::resetVs(VS newVs)
{
  // for NonSpatial we temporary hold the value in
  //  v, so we can change CR for a NonSpatial
  double v;
  if (isSpatial()) {
    // can not change to type of Spatial::d_val array
    PRECOND(allFitCRIndex(newVs) == allFitCRIndex(d_vs));
  } else
     getCell(v,0);
  d_vs=newVs;
  d_cri=allFitCRIndex(newVs);
  if (!isSpatial())
      setCell(v,0);
}

//! return a Field that can be written
/*!
 * \param v Field that can be a readOnlyReference
 * if \a v is a readOnlyReference then  a clone is returned
 * with readOnlyReference set to false
 */
calc::Field* calc::createDestCloneIfReadOnly(Field *v)
{
  if (v->readOnlyReference()) {
    Field *f=v->createClone();
    f->setReadOnlyReference(false);
    return f;
  }
  return v;
}

//! a non-efficient print for test-fields for debugging purposes only
std::ostream &operator<<(std::ostream& s, const calc::Field& f)
{
  s << "type(" << f.type() << ")";
  s << "nrValues(" << f.nrValues() << ")\n";
  s << "data(";
  for(size_t i=0; i < f.nrValues(); ++i) {
    double v;
    if (i)
      s << ",";
    if (f.getCell(v,i))
      s << v;
    else
      s << "mv";
  }
  s << ")\n";
  return s;
}
