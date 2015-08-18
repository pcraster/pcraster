#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_TIMERVALUE
#include "calc_timervalue.h"
#define INCLUDED_CALC_TIMERVALUE
#endif

// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_CALC_FIELDNRPARAMETER
#include "calc_fieldnrparameter.h"
#define INCLUDED_CALC_FIELDNRPARAMETER
#endif

#ifndef INCLUDED_CALC_ISCRIPT
#include "calc_iscript.h"
#define INCLUDED_CALC_ISCRIPT
#endif



/*!
  \file
  This file contains the implementation of the TimerValue class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC TIMERVALUE MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF TIMERVALUE MEMBERS
//------------------------------------------------------------------------------

calc::TimerValue::TimerValue()
{
}

calc::TimerValue::~TimerValue()
{
}

calc::TimerValue::TimerValue(const TimerValue& t):
      Symbol(t),
      d_vs(t.d_vs),
      d_value(t.d_value)
{
}

calc::TimerValue::TimerValue(
    const Symbol& c):
  Symbol(c)
{
  if (isNumber())
    d_value = toNumber();
  else {
    // may be a numeric binding
    // that is already set as a FieldNrParameter
   const FieldNrParameter *nrP =
     dynamic_cast<const FieldNrParameter *>(
       c.scriptConst().findSymbol(&c,VS_FIELD, false));
   if (!nrP || nrP->isArray()) {
     // maybe a string binding
     const Symbol *s=scriptConst().findBinding(c.name());
     if (s) // is a string binding, test48
       posError(qName()+" does not have a numeric value");
     // not known generate: undefined symbol test48a
     c.scriptConst().findSymbol(&c,VS_FIELD, true);
   }
   d_value=nrP->initialValue();
  }
  d_vs=vsOfNumber(d_value);
}

size_t calc::TimerValue::value() const
{
  return static_cast<size_t>(d_value);
}

size_t calc::TimerValue::check(const std::string& name) const
{
  if ( (!isIn(VS_N,d_vs)) || d_value < 0) // pcrcalc/test7a
       posError(qName()+" is not a whole positive number (in "+name+" definition)");
  return value();
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------


//! check if section is consistent defined
void calc::checkTimerSection(
    const TimerValue& start,
    const TimerValue& end,
    const TimerValue& slice)
{
  size_t timerStart = start.check("start time");
  size_t timerEnd   =   end.check("end time");
  size_t timerSlice = slice.check("time step");

  if (timerStart != 1) /* pcrcalc/test6 */
   end.posError("current limitation: start time must be 1 (not "+start.name()+")");
  if (timerSlice != 1) /* pcrcalc/test7 */
   slice.posError("current limitation: time step must be 1 (not "+slice.name()+")");
  if (timerStart > timerEnd) /* pcrcalc/test8 */
   start.posError("Start time ("+start.name()+") is greater than end time ("+end.name()+")");
}



