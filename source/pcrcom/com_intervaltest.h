#ifndef INCLUDED_COM_INTERVALTEST
#define INCLUDED_COM_INTERVALTEST



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



//! This class implements the unit tests for the Interval class.
class IntervalTest
{

public:

                   IntervalTest        ();

  void             setUp               ();

  void             tearDown            ();

  void             testBetweenLimits   ();

  void             testLessDoubleOperator();
  void             testLessOperator     ();
  void             testEqOperator       ();

  void             testMinMax          ();

  void             testFromLookupTableKeyCorrectFormat ();
  void             testFromLookupTableKeyWrongFormat ();
  void             testEqualTo ();

  void             testLimit           ();
  void             testRoundError      ();

  static boost::unit_test::test_suite* suite();

};

} // namespace com

#endif
