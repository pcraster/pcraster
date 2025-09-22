#include "stddefx.h"
#include "calc_tssoutputvalue.h"
#include "calc_map2csf.h"
#include "com_new.h"
#include "com_file.h"
#include "appargs.h"
#include "com_exception.h"

#include <algorithm>
#include <iomanip>

const size_t calc::TssOutputValue::maxCacheSize=128;

size_t calc::TssOutputValue::initNrRows() const
{
  if (appHeader == APP_NOHEADER) {
    // then write at last timestep, to support current tcl-interface
    return d_fw.nrTimeSteps();
  }
  if (d_fw.writeEachTimeStep())
    return 1;
  return std::min<size_t>(d_fw.nrTimeSteps(),maxCacheSize);
}

calc::TssOutputValue::TssOutputValue(const FileWriter& fw, size_t nrCols, VS vs):
  d_fw(fw),
  d_nrCols(nrCols),
  d_nrRows(initNrRows()),
  d_value(com::new2d<double>(d_nrRows,d_nrCols)),
  d_vs(vs)
{
  PRECOND(nrInSet(vs) == 1);
  PRECOND(d_nrCols > 0);
  PRECOND(d_nrRows > 0);

  fw.removeTssFile();
}

calc::TssOutputValue::~TssOutputValue()
{
  // if data left, write it otherwise not
  // this prevent the case that we only
  // write a header and no data
  if (d_nrRowsFilled > 0)
    flushToFile();
  com::delete2d<double>(d_value);
}

//! return ptr to values to be filled and nrVals
/*! returns 0 if current timestep does not have to be written
 */
double *calc::TssOutputValue::getValueBuffer(size_t &nrVals)
{
  /* pcrcalc/test13[cd] */
  if ( (!d_fw.writeCurrentTimeStep()) && (appHeader == APP_NOHEADER) )
    return nullptr;
  size_t t = d_fw.currentTimeStep();
  if (t == d_timeStepLastAdded) {
    // pcrcalc/test317 writing a value twice in loop!
    PRECOND(d_nrRowsFilled > 0);
    d_nrRowsFilled--;
  }
  //! record the last time step that is added
  d_timeStepLastAdded = t;

  if (d_nrRowsFilled == d_nrRows)
    flushToFile();
  nrVals = d_nrCols;
  return d_value[d_nrRowsFilled++];
}

void calc::TssOutputValue::openFile(std::ofstream& ofs)
{
  // begin, create file
  if (d_timeStepRow0 == 1) {
     com::open(ofs,d_fw.tssFileName());

    /* print header */
    CSF_VS vs = vs2CsfVs(d_vs);
    if (appHeader == APP_DEFHEADER) {
      if (vs == VS_UNDEFINED)
       ofs << "summary\n";
      else
       ofs << "timeseries " << toString(d_vs) << "\n";
      ofs << d_nrCols+1 << "\n"
          << "timestep\n";
      for(size_t c = 0; c < d_nrCols; c++)
       ofs << c+1 << "\n";
    }
  } else {
   // otherwise we open for appending
   com::open(ofs,d_fw.tssFileName(), std::ios::app);
  }
}

/*!
 * \bug
 *   this is called in dtor, while it can fail on disk full!
 */
void calc::TssOutputValue::flushToFile()
{
  std::ofstream f;
  openFile(f);

  const char *mvFmt = nullptr;
  int valFmt = 0;
  switch(static_cast<CSF_VS>(d_vs)) { // dangerous typecast
   case VS_LDD    :
   case VS_BOOLEAN: mvFmt = "1e31"; // len 4
                    valFmt = 4; break;
   case VS_ORDINAL:
   case VS_NOMINAL: mvFmt = "      1e31"; // len 10
                    valFmt = 10; break;
   default        : mvFmt = "       1e31"; // len 11
                    valFmt = 11; break;
                    // was %11.6g 6 is default precision
  }

  /* print each row of time table on new line in file */
  size_t timeStep = d_timeStepRow0;
  for( size_t l = 0; l < d_nrRowsFilled; l++,timeStep++) {
    if (appHeader == APP_NOHEADER) // select which to write
      while (!d_fw.writeThisTimeStep(timeStep))
        timeStep++;

    /* print time at begin of line */
    f << std::setw(8) << timeStep;

    /* print columns of row on line of file */
    for(size_t c = 0; c < d_nrCols; c++)
     if (IS_MV_REAL8(d_value[l]+c))
       f << mvFmt;
     else {
       double v = d_value[l][c];
       if (d_vs == VS_D)
         v = AppOutputDirection(v);
       f << " " << std::setw(valFmt) << v;
     }
    f << '\n';
    if (!f.good())
      throw com::FileError(d_fw.tssFileName(),"writing data failed");
  }

  d_nrRowsFilled = 0;
  d_timeStepRow0 = d_fw.currentTimeStep();
}
