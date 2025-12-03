#include "stddefx.h"
#include "calc_timeinputtssop.h"
#include "com_csfcell.h"
#include "calc.h"  // TIME_TABLE
#include "calc_runtimeenv.h"
#include "calc_execarguments.h"
#include "calc_timetable.h"
#include "calc_field.h"
#include "calc_domainerror.h"

#include <cmath>

/*!
  \file
  This file contains the implementation of the TimeinputTssOp class.
*/


//------------------------------------------------------------------------------

/*
namespace calc {

class TimeinputTssOpPrivate
{
public:

  TimeinputTssOpPrivate()
  {
  }

  ~TimeinputTssOpPrivate()
  {
  }

};

} // namespace calc
*/


//------------------------------------------------------------------------------
// DEFINITION OF STATIC TIMEINPUTTSSOP MEMBERS
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// DEFINITION OF TIMEINPUTTSSOP MEMBERS
//------------------------------------------------------------------------------

calc::TimeinputTssOp::TimeinputTssOp()
{
}

calc::TimeinputTssOp::~TimeinputTssOp()
{
}

/* NOT IMPLEMENTED
//! Assignment operator.
calc::TimeinputTssOp& calc::TimeinputTssOp::operator=(const TimeinputTssOp& rhs)
{
  if (this != &rhs) {
  }
  return *this;
}

//! Copy constructor. NOT IMPLEMENTED.
calc::TimeinputTssOp::TimeinputTssOp(const TimeinputTssOp& rhs):
  Base(rhs)
{
}
*/

/*
 * \todo
 *   refactor spatial/nonSpatial to one body, with a catch on DomainError
 */
void calc::TimeinputTssOp::exec(RunTimeEnv *rte, const Operator &op, size_t nrArgs) const
{
  ExecArguments a(op, rte, nrArgs);
  const Field &id(a[0]);

  const auto *tab = dynamic_cast<const calc::TimeTable *>(a.firstNonFieldInput());
  PRECOND(tab);
  const TIME_TABLE *tss = tab->tss();


  Field &r(a.createResult());

  if (!rte->timer().currentInt()) {
    throw DomainError("called outside dynamic section");
  }
  int const rowIndex = rte->timer().currentInt() - 1;
  if (rowIndex >= tss->nrSteps) {
    throw DomainError("timeseries too short");
  }

  try {

    if (!id.isSpatial()) {

      double v = NAN;
      id.getCell(v, 0);
      int const colNr = static_cast<int>(v);
      // no such column pcrcalc232
      if (colNr <= 0 || colNr >= tss->nrCols) {
        throw DomainError("No match");
      }

      REAL8 *vPtr = tss->vals[rowIndex] + colNr;
      if (IS_MV_REAL8(vPtr)) {  // pcrcalc37e
        throw DomainError("Read mv for non-spatial");
      }
      PRECOND(!r.isSpatial());
      r.setCell(*vPtr, 0);

    } else {

      for (size_t i = 0; i < id.nrValues(); ++i) {
        double v = NAN;
        id.getCell(v, i);
        int const colNr = static_cast<int>(v);

        if (colNr <= 0 || colNr >= tss->nrCols) {
          // out of range  TODO setMV of Field
          SET_MV_REAL8(&v);
          r.setCell(v, i);
        } else {
          REAL8 *vPtr = tss->vals[rowIndex] + colNr;
          r.setCell(*vPtr, i);
        }
      }
    }

  } catch (std::exception &v) {
    // geen idee wat hier opgevangen wordt
    throw DomainError(v.what());
  }
  a.pushResults();
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------
