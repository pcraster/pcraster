#include "stddefx.h"
#include "calc_writeinfo.h"
#include "calc_report.h"
#include "calc_iscript.h"

/*!
 * If hasReportPrefix is true but report is 0 then the default report is used
 */
calc::WriteInfo::WriteInfo(const IScript *script, bool hasReportPrefix, const Report *report,
                           bool inDynamic)
    : d_script(script), d_hasReportPrefix(hasReportPrefix), d_report(report), d_inDynamic(inDynamic)
{
  if ((d_report == nullptr) && d_hasReportPrefix) {
    d_report = d_script->reportDefault();
  }
}

bool calc::WriteInfo::isWritten() const
{
  return d_report != nullptr || d_script->allIsWritten();
}

const calc::Report *calc::WriteInfo::report() const
{
  if (d_report != nullptr) {
    return d_report;
  }
  return d_script->reportDefault();
}

bool calc::WriteInfo::hasReportPrefix() const
{
  return d_hasReportPrefix;
}

bool calc::WriteInfo::inDynamic() const
{
  return isWritten() && d_inDynamic;
}

bool calc::WriteInfo::writeAtTimestep(size_t t) const
{
  return isWritten() && report()->reportTimestep(t);
}
