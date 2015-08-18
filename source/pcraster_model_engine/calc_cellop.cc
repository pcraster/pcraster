#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_CELLOP
#include "calc_cellop.h"
#define INCLUDED_CALC_CELLOP
#endif

// Library headers.

// PCRaster library headers.
#ifdef DEBUG_DEVELOP
#ifndef INCLUDED_COM_CSFCELL
#include "com_csfcell.h"
#define INCLUDED_COM_CSFCELL
#endif
#endif

// Module headers.
#ifndef INCLUDED_CALC_RUNTIMEENV
#include "calc_runtimeenv.h"
#define INCLUDED_CALC_RUNTIMEENV
#endif
#ifndef INCLUDED_CALC_ICELLITERATOR
#include "calc_icelliterator.h"
#define INCLUDED_CALC_ICELLITERATOR
#endif
#ifndef INCLUDED_CALC_NONSPATIAL
#include "calc_nonspatial.h"
#define INCLUDED_CALC_NONSPATIAL
#endif



/*!
  \file
  This file contains the implementation of the CellOp class.
*/



//------------------------------------------------------------------------------


namespace calc {
CellFocus         builtIn__cellfocus;
LddDownstreamCell builtIn__ldddownstreamcell;

} // namespace calc

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
  Field *f=rte->popField();
  if (f->isSpatial()) {
    double value;
    f->getCell(value,i);
    // Executor::visitCellIterator ensures this
    DEVELOP_PRECOND(f->getCell(value,i));
    rte->pushField(new NonSpatial(f->vs(),value));
    delete f;
  } else
    rte->pushField(f);
}

void calc::CellFocus::exec(RunTimeEnv *rte, const Operator&, size_t) const
{
  getCell(rte,rte->cellIterator()->current());
}

void calc::LddDownstreamCell::exec(RunTimeEnv *rte, const Operator&, size_t) const
{
  getCell(rte,rte->cellIterator()->lddDownstream());
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



