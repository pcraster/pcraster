#include "stddefx.h"
#include "calc_timetable.h"
#include "calc.h"  // Table related functions
#include "com_exception.h"
#include "calc_globallibdefs.h"
#include "calc_map2csf.h"
#include "calc_astsymbolinfo.h"
#include "calc_quote.h"

void calc::TimeTable::init(const std::string &fileName, VS resultFieldVs, size_t nrTimeStepsExpected)
{
  d_tss = nullptr;
  try {
    d_tss = ReadTimeInputTable(fileName.c_str(), 0, 0, vs2CsfVs(resultFieldVs));
    if (d_tss == nullptr) {
      libError("");
    }

    // pcrcalc226
    if (d_tss->nrSteps < (int)nrTimeStepsExpected) {
      throw com::Exception("Timeseries " + quote(fileName) + " contains only " + quote(d_tss->nrSteps) +
                           " entries (" + quote(nrTimeStepsExpected) + " required)");
    }
  } catch (...) {
    clean();
    throw;
  }
}

//! construct by input tss file and read tss
/*!
 * checks on number of steps required
 * \exception com::Exception in case of (format) error
 * \bug
 *   app/readcols.c has a bug, for each tss a single (possible last) chunk
 *   allocated in NewAllRec() is never freed, refactor readcols.c
 */
calc::TimeTable::TimeTable(const std::string &fileName, VS resultFieldVs, size_t nrTimeStepsExpected)
{
  init(fileName, resultFieldVs, nrTimeStepsExpected);
}

//! identical to other ctor
calc::TimeTable::TimeTable(const ASTSymbolInfo &s, size_t nrTimeStepsExpected)
{
  init(s.externalName(), s.dataType().resultType(), nrTimeStepsExpected);
}

//! return the actual tss struct
const TIME_TABLE *calc::TimeTable::tss() const
{
  return d_tss;
}

//! delete the TIME_TABLE struct
void calc::TimeTable::clean()
{
  if (d_tss != nullptr) {
    FreeTimeTable(d_tss);
  }
  d_tss = nullptr;
}

//! dtor
calc::TimeTable::~TimeTable()
{
  clean();
}

//! nr of columns including the time column
size_t calc::TimeTable::nrCols() const
{
  return d_tss->nrCols;
}

//! nr of time steps
size_t calc::TimeTable::nrTimeSteps() const
{
  return d_tss->nrSteps;
}

calc::OVS calc::TimeTable::ovs() const
{
  return VS_TSS;
}
