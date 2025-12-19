#ifndef INCLUDED_CALC_TIMETABLE
#define INCLUDED_CALC_TIMETABLE

#include "calc_types.h"
#include "calc_datavalue.h"

#include <string>


struct TIME_TABLE;

namespace calc {

class ASTSymbolInfo;

//! a input timeseries
/*! This class uses the 'old' TIME_TABLE structure
 */
class TimeTable : public DataValue {
 private:
  TIME_TABLE *d_tss{};

  void clean();
  void init(const std::string& fileName,
            VS                 resultFieldVs,
            size_t nrTimeStepsExpected);

 public:

  TimeTable(const ASTSymbolInfo& s,size_t nrTimeStepsExpected=1);
  TimeTable(const std::string& fileName,
            VS                 resultFieldVs,
            size_t nrTimeStepsExpected=1);


  ~TimeTable() override;

  size_t nrCols() const;
  size_t nrTimeSteps() const;

  const struct TIME_TABLE *tss() const;

  OVS   ovs() const override;

};

}

#endif
