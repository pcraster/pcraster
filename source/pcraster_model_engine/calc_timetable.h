#ifndef INCLUDED_CALC_TIMETABLE
#define INCLUDED_CALC_TIMETABLE

#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif
#ifndef INCLUDED_CALC_TYPES
#include "calc_types.h"
#define INCLUDED_CALC_TYPES
#endif
#ifndef INCLUDED_CALC_DATAVALUE
#include "calc_datavalue.h"
#define INCLUDED_CALC_DATAVALUE
#endif

struct TIME_TABLE;

namespace calc {

class ASTSymbolInfo;

//! a input timeseries
/*! This class uses the 'old' TIME_TABLE structure
 */
class TimeTable : public DataValue {
 private:
  TIME_TABLE *d_tss;

  void clean();
  void init(const std::string& fileName,
            VS                 resultFieldVs,
            size_t nrTimeStepsExpected);

 public:

  TimeTable(const ASTSymbolInfo& s,size_t nrTimeStepsExpected=1);
  TimeTable(const std::string& fileName,
            VS                 resultFieldVs,
            size_t nrTimeStepsExpected=1);


  ~TimeTable();

  size_t nrCols() const;
  size_t nrTimeSteps() const;

  const struct TIME_TABLE *tss() const;

  OVS   ovs() const;

};

}

#endif
