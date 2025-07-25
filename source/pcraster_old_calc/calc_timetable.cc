#include "stddefx.h"
#include "calc_timetable.h"
#include "calc_liberror.h"
#include "calc_map2csf.h"
#include "calc.h"    // Table related functions
#include "com_exception.h"
#include "calc_quote.h"

//! construct by input tss file and read tss
/*!
 * checks on number of steps required
 * \exception com::Exception in case of (format) error
 * \bug
 *   app/readcols.c has a bug, for each tss a single (possible last) chunk
 *   allocated in NewAllRec() is never freed, refactor readcols.c
 * \todo
 *    parsing and reading of tss file is done, during
 *      syntax check, can take long with big tss, move
 *      to a later phase (?)
 */
calc::TimeTable::TimeTable(
  const std::string &fileName,
  VS vs,
  size_t nrTimeStepsExpected):
  d_vs(vs),
  d_tss(ReadTimeInputTable(fileName.c_str(), 0,0, vs2CsfVs(vs)))
{
  try {
  if (!d_tss)
       libError("");

  // pcrcalc/test226
  if (d_tss->nrSteps < (int)nrTimeStepsExpected)
    throw com::Exception("Timeseries "+quote(fileName)+" contains only "+
      quote(d_tss->nrSteps)+" entries ("+quote(nrTimeStepsExpected)+" required)");
  } catch (...) {
    cleanUp();
    throw;
  }
}

//! construct by input tss file and read tss
/*!
 * \exception com::Exception in case of (format) error
 * \todo
 *    parsing and reading of tss file is done, during
 *      syntax check, can take long with big tss, move
 *      to a later phase (?)
 */
calc::TimeTable::TimeTable(
  const std::string &fileName):
  d_vs(VS_S),
  d_tss(ReadTimeInputTable(fileName.c_str(), 0,0, vs2CsfVs(VS_S)))
{
  try {
  if (!d_tss)
       libError("");
  } catch (...) {
    cleanUp();
    throw;
  }
}

//! return the actual tss struct
const TIME_TABLE *calc::TimeTable::tss() const
{ return d_tss; }

//! delete the TIME_TABLE struct
void calc::TimeTable::cleanUp()
{
  if (d_tss)
    FreeTimeTable(d_tss);
  d_tss = nullptr;
}

//! dtor
calc::TimeTable::~TimeTable()
{
  cleanUp();
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


//! vs of data
VS calc::TimeTable::vs() const
{
  return d_vs;
}
