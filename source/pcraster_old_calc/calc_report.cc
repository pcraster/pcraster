#include "stddefx.h"

#ifndef INCLUDED_CALC_REPORT
#include "calc_report.h"
#define INCLUDED_CALC_REPORT
#endif

#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif

calc::Report::Report()
{
}

calc::Report::~Report()
{
}

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

bool calc::ReportDefinition::reportTimestep(size_t  t) const
{
  DEVELOP_PRECOND(t < d_reportAt.size());
  return d_reportAt[t];
}

calc::ReportDefinition::ReportDefinition(const calc::Symbol& s,const std::vector<ParsReportMoment>& list,
  int endTime):
  calc::Symbol(s),
  // set all false, inclusive
  d_reportAt(endTime+1,false)
{
  // set the report for static model
  d_reportAt[0]=false;

  for(size_t l=0; l < list.size(); l++) {
    ParsReportMoment m = list[l];
    int i;
    if (m.start > endTime) // single or range outside 1..endTime
      continue; // do not add
    if (m.start == -1) // keyword "endtime"
      m.start = endTime;
    if (m.end  == -1) // keyword "endtime"
      m.end = endTime;
    if (m.end > endTime) // forget other timesteps pcrcalc/test234c
      m.end = endTime;

    if (m.end == 0) // single
      d_reportAt[m.start]=true;
    else {  // range
      if (m.step == 0)
        m.step = 1;
      PRECOND(m.start <= m.end && m.step >= 1);
      for(i=m.start; i <= m.end; i+=m.step) {
        if (i == 0) // see AdjustStackMinMax
                    // and test pcrcalc/234a
          continue;
        d_reportAt[i]=true;
      }
    }
  }
}

calc::ReportDefinition::~ReportDefinition()
{
}


calc::ReportDefault::ReportDefault():
   d_definition(0)
{
}

calc::ReportDefault::~ReportDefault()
{
}

void calc::ReportDefault::setDefinition(const ReportDefinition *definition)
{
  d_definition=definition;
}

bool calc::ReportDefault::reportTimestep(size_t  t) const
{
  // pcrcalc/test234d: t >= 1: definition is only for dynamic
  if (d_definition && t >= 1)
    return d_definition->reportTimestep(t);
  // otherwise report at each timestep
  return true;
}
