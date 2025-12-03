#include "stddefx.h"
#include "calc_cellop.h"
#ifdef DEBUG_DEVELOP
#include "com_csfcell.h"
#endif
#include "calc_runtimeenv.h"
#include "calc_icelliterator.h"
#include "calc_nonspatial.h"

#include <cmath>

/*!
  \file
  This file contains the implementation of the CellOp class.
*/


//------------------------------------------------------------------------------


namespace calc
{
CellFocus builtIn__cellfocus;
LddDownstreamCell builtIn__ldddownstreamcell;

}  // namespace calc

//------------------------------------------------------------------------------
// DEFINITION OF STATIC CELLOP MEMBERS
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// DEFINITION OF CELLOP MEMBERS
//------------------------------------------------------------------------------

calc::CellOp::CellOp()
{
}

/* NOT IMPLEMENTED
//! Copy constructor.
calc::CellOp::CellOp(CellOp const& rhs)

  : Base(rhs)

{
}
*/


calc::CellOp::~CellOp()
{
}

/* NOT IMPLEMENTED
//! Assignment operator.
calc::CellOp& calc::CellOp::operator=(CellOp const& rhs)
{
  if (this != &rhs) {
  }
  return *this;
}
*/

void calc::CellOp::getCell(RunTimeEnv *rte, size_t i) const
{
  Field *f = rte->popField();
  if (f->isSpatial()) {
    double value = NAN;
    f->getCell(value, i);
    // Executor::visitCellIterator ensures this
    DEVELOP_PRECOND(f->getCell(value, i));
    rte->pushField(new NonSpatial(f->vs(), value));
    delete f;
  } else {
    rte->pushField(f);
  }
}

void calc::CellFocus::exec(RunTimeEnv *rte, const Operator &, size_t) const
{
  getCell(rte, rte->cellIterator()->current());
}

void calc::LddDownstreamCell::exec(RunTimeEnv *rte, const Operator &, size_t) const
{
  getCell(rte, rte->cellIterator()->lddDownstream());
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------
