#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_REPORT
#include "calc_report.h"
#define INCLUDED_CALC_REPORT
#endif

// Library headers.

// PCRaster library headers.
#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif

// Module headers.
#ifndef INCLUDED_CALC_QUOTE
#include "calc_quote.h"
#define INCLUDED_CALC_QUOTE
#endif

#ifndef INCLUDED_CALC_POSITIONNAME
#include "calc_positionname.h"
#define INCLUDED_CALC_POSITIONNAME
#endif
#ifndef INCLUDED_CALC_TIMER
#include "calc_timer.h"
#define INCLUDED_CALC_TIMER
#endif




/*!
  \file
  This file contains the implementation of the Report class.
*/



//------------------------------------------------------------------------------

/*
namespace calc {

class ReportPrivate
{
public:

  ReportPrivate()
  {
  }

  ~ReportPrivate()
  {
  }

};

} // namespace calc
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC REPORT MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF REPORT MEMBERS
//------------------------------------------------------------------------------

//! check if valid
/*!
 * \throws com::Exception  if not valid
 */
void calc::ParsReportMoment::check()
{
  if (start <= end
      || end <= 0 // end is 0,single point, or endtime (-1)
     )
    return; // OK

  char buf[128];
  if (step == 0)
    sprintf(buf,"%d-%d",start,end);
  else
    sprintf(buf,"%d+%d-%d",start,step,end);

  /* pcrcalc/test24[234] */
  throw com::Exception("report moment contains invalid range "+quote(buf));
}

calc::Report::Report():
  // past end
  d_startInt(1),
  d_lastInt(1)
{
}



calc::Report::~Report()
{
}

//! Assignment operator.
calc::Report& calc::Report::operator=(const Report& rhs)
{
  if (this != &rhs) {
   d_reportAt=rhs.d_reportAt;
   d_list    = rhs.d_list;
   d_startInt=rhs.d_startInt;
   d_lastInt =rhs.d_lastInt;
  }
  return *this;
}

//! Copy constructor.
calc::Report::Report(const Report& rhs):
  ASTId(rhs),
  d_reportAt(rhs.d_reportAt),
  d_list(rhs.d_list),
  d_startInt(rhs.d_startInt),
  d_lastInt(rhs.d_lastInt)
{
  PRECOND(!d_list.empty());
}

//! fill in the end time
void calc::Report::update(const Timer& timer)
{
  PRECOND(!d_list.empty());

  int endTime=timer.lastInt();
  d_startInt =timer.startInt();
  d_lastInt  =timer.lastInt();

  d_reportAt.clear();
  // set all false, inclusive
  d_reportAt.resize(endTime+1,false);
  // set the report for static model
  d_reportAt[0]=false;

  for(size_t l=0; l < d_list.size(); l++) {
    ParsReportMoment m = d_list[l];
    if (m.start > endTime) // single or range outside 1..endTime
      continue; // do not add
    if (m.start == -1) // keyword "endtime"
      m.start = endTime;
    if (m.end  == -1) // keyword "endtime"
      m.end = endTime;
    if (m.end > endTime) // forget other timesteps pcrcalc/test234c
      m.end = endTime;

    if (m.end == 0) {
      if (m.start >= (int)timer.startInt()) // single
         d_reportAt[m.start]=true;
    } else {  // range
      if (m.step == 0)
        m.step = 1;
      PRECOND(m.start <= m.end && m.step >= 1);
      for(size_t i=m.start; i <= (size_t)m.end; i+=m.step)
         if (i >= timer.startInt()) {
            // see AdjustStackMinMax
            // and test pcrcalc/234a
          d_reportAt[i]=true;
         }
    }
  }
}

#ifdef DEBUG_DEVELOP
void calc::Report::print() const
{
  std::string s;
  for(size_t i=0; i < d_reportAt.size(); ++i)
    s += d_reportAt[i] ? '1' : '0';
  PRINT_VAR(s);
}
#endif

//! generate the system generated used default
calc::Report calc::Report::reportDefault()
{
  PositionName pn("reportDefaultUserGenerated");
  Id id("reportdefault",&pn);
  PL list;
  ParsReportMoment m = {1,0,-1};
  list.push_back(m);
  return Report(id,list);
}

//! should int timestep \a t be reported?
bool calc::Report::atInt(size_t  t) const
{
  // this will also fail if update is never called
  PRECOND(t < d_reportAt.size());
  return d_reportAt[t];
/* from reportDefault
  // pcrcalc/test234d: t >= 1: definition is only for dynamic
  if (d_definition && t >= 1)
    return d_definition->atInt(t);
  // otherwise report at each timestep
  return true;
*/
}

size_t calc::Report::startInt() const
{
  return d_startInt;
}

size_t calc::Report::lastInt() const
{
  return d_lastInt;
}

calc::Report::Report(
    const Id& s,
    const PL& list):
  ASTId(s),
  d_list(list)
{
}

void calc::Report::accept(ASTVisitor& /*v*/)
{
}

calc::Report* calc::Report::createClone()const
{
  return new Report(*this);
}


//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

