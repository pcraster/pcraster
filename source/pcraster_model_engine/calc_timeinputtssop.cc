#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_TIMEINPUTTSSOP
#include "calc_timeinputtssop.h"
#define INCLUDED_CALC_TIMEINPUTTSSOP
#endif

// Library headers.

// PCRaster library headers.
#ifndef INCLUDED_COM_CSFCELL
#include "com_csfcell.h"
#define INCLUDED_COM_CSFCELL
#endif
#ifndef INCLUDED_CALC
#include "calc.h" // TIME_TABLE
#define INCLUDED_CALC
#endif

// Module headers.
#ifndef INCLUDED_CALC_RUNTIMEENV
#include "calc_runtimeenv.h"
#define INCLUDED_CALC_RUNTIMEENV
#endif
#ifndef INCLUDED_CALC_EXECARGUMENTS
#include "calc_execarguments.h"
#define INCLUDED_CALC_EXECARGUMENTS
#endif
#ifndef INCLUDED_CALC_TIMETABLE
#include "calc_timetable.h"
#define INCLUDED_CALC_TIMETABLE
#endif
#ifndef INCLUDED_CALC_FIELD
#include "calc_field.h"
#define INCLUDED_CALC_FIELD
#endif

#ifndef INCLUDED_CALC_DOMAINERROR
#include "calc_domainerror.h"
#define INCLUDED_CALC_DOMAINERROR
#endif


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
void calc::TimeinputTssOp::exec(RunTimeEnv* rte,const Operator& op,size_t nrArgs) const
{
  ExecArguments a(op,rte,nrArgs);
  const Field& id(a[0]);

  const calc::TimeTable *tab=
    dynamic_cast<const calc::TimeTable *>(a.firstNonFieldInput());
  PRECOND(tab);
  const TIME_TABLE *tss=tab->tss();


  Field& r(a.createResult());

  if (!rte->timer().currentInt())
     throw DomainError("called outside dynamic section");
  int rowIndex = rte->timer().currentInt()-1;
  if (rowIndex >= tss->nrSteps)
     throw DomainError("timeseries too short");

  try {

   if (!id.isSpatial()) {

      double v;
      id.getCell(v,0);
      int colNr = static_cast<int>(v);
      // no such column pcrcalc232
      if (colNr <= 0 || colNr >= tss->nrCols)
        throw DomainError("No match");

      REAL8 *vPtr=tss->vals[rowIndex]+colNr;
      if (IS_MV_REAL8(vPtr)) // pcrcalc37e
        throw DomainError("Read mv for non-spatial");
      PRECOND(!r.isSpatial());
      r.setCell(*vPtr,0);

   } else {

      for(size_t i=0; i < id.nrValues(); ++i) {
       double v;
       id.getCell(v,i);
       int colNr = static_cast<int>(v);

       if (colNr <= 0 || colNr >= tss->nrCols) {
         // out of range  TODO setMV of Field
         SET_MV_REAL8(&v);
         r.setCell(v,i);
       } else {
         REAL8 *vPtr=tss->vals[rowIndex]+colNr;
         r.setCell(*vPtr,i);
       }
      }
   }

   }  catch(std::exception& v) {
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
