#ifndef INCLUDED_CALC_TSSOUTPUTVALUE
#define INCLUDED_CALC_TSSOUTPUTVALUE

#ifndef INCLUDED_FSTREAM
#include <fstream>
#define INCLUDED_FSTREAM
#endif
#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
#endif
#ifndef INCLUDED_BOOST_NONCOPYABLE
#include <boost/noncopyable.hpp>
#define INCLUDED_BOOST_NONCOPYABLE
#endif

#ifndef INCLUDED_CALC_VS
#include "calc_vs.h"
#define INCLUDED_CALC_VS
#endif

#ifndef INCLUDED_CALC_STACKINFO
#include "calc_stackinfo.h"
#define INCLUDED_CALC_STACKINFO
#endif

namespace calc {

class Field;
class IOStrategy;

//! An output tss
class TssOutputValue : public boost::noncopyable {
 protected:

  TssOutputValue();
 public:

 virtual ~TssOutputValue();

  virtual void    finish()=0;

  virtual void timeoutput(const Field *id,
                          const Field *expr,
                          size_t currentTimeStep)=0;
};

//! An output tss file
/*! An output tss is either the result of
 *  <ul>
 *   <li>dynamic non spatial output (NSTssWriter)</li>
 *   <li>a timeoutput statement (Timeoutput)</li>
 *  </ul>
 *  File is created at the report of the first timestep that
 *  must be flushed, successive timesteps are then appended to the file.
 */
class FileTimeoutput : public TssOutputValue {
 private:
  static const size_t maxCacheSize;

  size_t initNrRowsCached() const;

  StackInfo  d_stackInfo;

  //! maximum value in id map during first timestep
  size_t d_nrCols;
  //! nr of allocated rows, in d_value
  size_t d_nrRowsCached;

  bool   d_fileErrorOccured;
  bool   d_fileCreated;

  size_t d_lastStepToFile;

  //! holding gives timestep of same index in d_value
  //  size equals nr of filled rows in d_value
  std::vector<size_t>   d_step;

  double **d_value;

  void flushToFile();

  void openFile(std::ofstream& ofs);

  double *setRow(size_t currentTimeStep);

  bool reportTimeStep(size_t t) const;

  void printLine(size_t         t,
                 double        *v,
                 std::ofstream& f);

 public:
  FileTimeoutput(const StackInfo& stackInfo, size_t nrCols);

  ~FileTimeoutput();

  void    finish();

  void timeoutput(const Field *id, const Field *expr, size_t currentTimeStep);
  void nonspatial(const Field *f,                     size_t currentTimeStep);
};

class MemoryTimeoutput : public TssOutputValue {
private:
    std::string d_name;
    size_t      d_memoryId;
    IOStrategy& d_ios;
public:
  MemoryTimeoutput(
    const std::string& name,
    size_t             memoryId,
    IOStrategy&    ios);

  void    finish();
  void timeoutput(const Field *id, const Field *expr, size_t currentTimeStep);
};

FileTimeoutput* createFileTimeoutput(
    const  StackInfo& stackInfo,
    const Field* id);

}
#endif
