#ifndef INCLUDED_CALC_REPORT
#define INCLUDED_CALC_REPORT

#ifndef INCLUDED_CALC_SYMBOL
#include "calc_symbol.h"
#define INCLUDED_CALC_SYMBOL
#endif

#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
#endif

namespace calc {

//! define the moment
typedef struct ParsReportMoment {
  //! the single value, or the start of step (inclusive)
  int start;
  //! the step increase, 0 if not set by user
  /*! no step given by user (1 is default,so 0 -> 1)
    */
  int step;
  /* the end of the step (inclusive),0 marks a single value
   * stored in start
   */
  int end;
  //! verify correctness

  void check(void);
} ParsReportMoment;

//! holds at which timesteps data is written
class Report {
  public:
            Report();
   virtual ~Report();
   virtual bool reportTimestep(size_t  t) const=0;
};

//! a definition as it appears in the timer section
class ReportDefinition: public Report, public Symbol {
private:
  std::vector<bool> d_reportAt;

public:
  // CREATORS
  ReportDefinition(const Symbol& s,const std::vector<ParsReportMoment>& list, int endTime);
  ~ReportDefinition();

  // ACCESSORS

  //! is report set at timestep t
  bool reportTimestep(size_t  t) const;
};

//! how reports by default are handled
class ReportDefault : public Report {
  /*! a (re-)definition is present if not 0
   * this does not own d_definition
   */
  const ReportDefinition  *d_definition;
  public:
             ReportDefault();
            ~ReportDefault();
  void       setDefinition(const ReportDefinition *definition);
   bool      reportTimestep(size_t  t) const;
};

}

#endif
