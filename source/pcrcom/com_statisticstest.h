#ifndef INCLUDED_COM_STATISTICSTEST
#define INCLUDED_COM_STATISTICSTEST



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

namespace com {
  // Statistics declarations.
}



namespace com {



//! This class implements the unit tests for the Statistics class.
class StatisticsTest
{

public:

                   StatisticsTest      ();

  void             setUp               ();

  void             tearDown            ();

  void             testSum             ();
  void             testSumNr           ();
  void             testAverage         ();
  void             testAverageMinMax   ();
  void             testVariance1       ();
  void             testVariance2       ();
  void             testStandardDeviation();
  void             testAverageSdMinMax ();
  void             testPercentile ();
  void             testSuse();

  static boost::unit_test::test_suite* suite();

};

} // namespace com

#endif
