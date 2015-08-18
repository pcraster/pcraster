#include "stddefx.h"

#ifndef INCLUDED_CALC_WRITEINFO
#include "calc_writeinfo.h"
#define INCLUDED_CALC_WRITEINFO
#endif

#ifndef INCLUDED_CALC_REPORT
#include "calc_report.h"
#define INCLUDED_CALC_REPORT
#endif

#ifndef INCLUDED_CALC_ISCRIPT
#include "calc_iscript.h"
#define INCLUDED_CALC_ISCRIPT
#endif

/*!
 * If hasReportPrefix is true but report is 0 then the default report is used
 */
calc::WriteInfo::WriteInfo(
  const IScript   *script,
  bool             hasReportPrefix,
  const Report    *report,
  bool             inDynamic):
   d_script(script),
   d_hasReportPrefix(hasReportPrefix),d_report(report),d_inDynamic(inDynamic)
{
  if ((!d_report) && d_hasReportPrefix) {
    d_report = d_script->reportDefault();
  }
}

bool calc::WriteInfo::isWritten() const
{
  return d_report != 0 || d_script->allIsWritten();
}

const calc::Report *calc::WriteInfo::report() const
{
  if (d_report != 0 )
    return d_report;
  return d_script->reportDefault();
}


bool calc::WriteInfo::hasReportPrefix() const
{
  return d_hasReportPrefix;
}

bool calc::WriteInfo::inDynamic() const
{
  return isWritten() &&  d_inDynamic;
}


bool calc::WriteInfo::writeAtTimestep(size_t t) const
{
  return isWritten() &&  report()->reportTimestep(t);
}
