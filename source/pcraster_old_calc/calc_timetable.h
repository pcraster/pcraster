#ifndef INCLUDED_CALC_TIMETABLE
#define INCLUDED_CALC_TIMETABLE

#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif

#ifndef INCLUDED_VSENUM
#include "vsenum.h"
#define INCLUDED_VSENUM
#endif

struct TIME_TABLE;

namespace calc {

//! a input timeseries
/*! This class uses the 'old' TIME_TABLE structure
 */
class TimeTable {
 private:
  //! vs of data
  const VS d_vs;
  //! the value holder
  TIME_TABLE *d_tss;


  void cleanUp();

 public:
  TimeTable(const std::string &inputTssFile,
    VS type,size_t nrTimeStepsExpected);
  TimeTable(const std::string &inputTssFile);

  ~TimeTable();

  size_t nrCols() const;
  size_t nrTimeSteps() const;

  const struct TIME_TABLE *tss() const;

  VS vs() const;
};

}

#endif
