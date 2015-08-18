#ifndef INCLUDED_CALC_TSSOUTPUTVALUE
#define INCLUDED_CALC_TSSOUTPUTVALUE

#ifndef INCLUDED_FSTREAM
#include <fstream>
#define INCLUDED_FSTREAM
#endif

#ifndef INCLUDED_CALC_VS
#include "calc_vs.h"
#define INCLUDED_CALC_VS
#endif

#ifndef INCLUDED_CALC_FILEWRITER
#include "calc_filewriter.h"
#define INCLUDED_CALC_FILEWRITER
#endif

namespace calc {
//! An output tss
/*! An output tss is either the result of
 *  <ul>
 *   <li>dynamic non spatial output (NonSpatialTssImpl)</li>
 *   <li>a timeoutput statement (TssOutputParameter)</li>
 *  </ul>
 */
class TssOutputValue {
 private:
   static const size_t maxCacheSize;

  size_t initNrRows() const;

  FileWriter d_fw;

  //! maximum value in id map during first timestep
  size_t d_nrCols;
  //! nr of allocated rows, in d_value
  size_t d_nrRows;

  //! nr of current rows filled
  size_t d_nrRowsFilled;

  //! the timestep equal or less then the one store in row 0
  /*! it may be the case that we do now write this timestep
   *  but it is the value where we start to look if we want
   *  to write
   */
  size_t d_timeStepRow0;
  //! record the last time step that is added 
  size_t d_timeStepLastAdded;

  double **d_value;

  //! the vs of the timeseries
  VS d_vs;

  void flushToFile();

  void openFile(std::ofstream& ofs);

 public:
  TssOutputValue(const FileWriter& fw, size_t nrCols, VS vs);

  ~TssOutputValue();

  //! return ptr to values to be filled and nrVals
  double *getValueBuffer(size_t &nrVals);
};

}

#endif
