#include "stddefx.h"


#ifndef INCLUDED_CALC_FIELDEXPR
#include "calc_fieldexpr.h"
#define INCLUDED_CALC_FIELDEXPR
#endif

#ifndef INCLUDED_CALC_FIELDTYPE
#include "calc_fieldtype.h"
#define INCLUDED_CALC_FIELDTYPE
#endif

#ifndef INCLUDED_CALC_SPATIAL
#include "calc_spatial.h"
#define INCLUDED_CALC_SPATIAL
#endif

#ifndef INCLUDED_CALC_NONSPATIAL
#include "calc_nonspatial.h"
#define INCLUDED_CALC_NONSPATIAL
#endif

#ifndef INCLUDED_CALC_ISCRIPT
#include "calc_iscript.h"
#define INCLUDED_CALC_ISCRIPT
#endif

#ifndef INCLUDED_CALC_COMPRESSOR
#include "calc_compressor.h"
#define INCLUDED_CALC_COMPRESSOR
#endif

calc::FieldExpr::FieldExpr(
  const calc::Element& name):
  calc::Element(name)
{
}

calc::FieldExpr::~FieldExpr()
{
}
VS calc::FieldExpr::vs() const
{
  return fieldType().vs();
}

bool calc::FieldExpr::spatial() const
{
  return fieldType().spatial();
}

bool calc::FieldExpr::isConstant() const
{
  return false;
}

bool calc::FieldExpr::isFieldLeaf() const
{
  return false;
}

bool calc::FieldExpr::isEndNode() const
{
  return isFieldLeaf() || isConstant();
}

calc::FieldHandle calc::FieldExpr::createResultField() const
{
  VS v = biggestVs(vs());
  if (spatial())
    return new Spatial(v,compressor().nrCellsCompressed(),true);
  return new NonSpatial(v);
}

const calc::Compressor& calc::FieldExpr::compressor() const
{
  return scriptConst().compressor();
}
