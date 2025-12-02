#include "stddefx.h"
#include "calc_fieldexpr.h"
#include "calc_fieldtype.h"
#include "calc_spatial.h"
#include "calc_nonspatial.h"
#include "calc_iscript.h"
#include "calc_compressor.h"

calc::FieldExpr::FieldExpr(const calc::Element &name) : calc::Element(name)
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
  VS const v = biggestVs(vs());
  if (spatial())
    return new Spatial(v, compressor().nrCellsCompressed(), true);
  return new NonSpatial(v);
}

const calc::Compressor &calc::FieldExpr::compressor() const
{
  return scriptConst().compressor();
}
