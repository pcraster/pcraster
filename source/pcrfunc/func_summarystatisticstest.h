#ifndef INCLUDED_FUNC_SUMMARYSTATISTICSTEST
#define INCLUDED_FUNC_SUMMARYSTATISTICSTEST



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.

// Module headers.



namespace boost {
  namespace unit_test {
    class test_suite;
  }
}

namespace func {
  // SummaryStatisticsTest declarations.
}



namespace func {

//! This class implements the unit tests for the SummaryStatistics class.
class SummaryStatisticsTest
{

private:

public:

                   SummaryStatisticsTest();

  void             setUp               ();

  void             tearDown            ();

  void             testMean            ();

  static boost::unit_test::test_suite* suite();

};

} // namespace func

#endif
