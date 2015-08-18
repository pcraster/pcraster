#ifndef INCLUDED_DAL_MATHUTILSTEST
#define INCLUDED_DAL_MATHUTILSTEST



// Library headers.

// PCRaster library headers.

// Module headers.



namespace boost {
  namespace unit_test {
    class test_suite;
  }
}

namespace dal {
  // MathUtilsTest declarations.
}



namespace dal {

//! This class implements the unit tests for the MathUtils class.
class MathUtilsTest
{

private:

public:

                   MathUtilsTest       ();

  void             setUp               ();

  void             tearDown            ();

  void             testClamp           ();

  void             testIsRegularIncreasingRange();

  void             testIsIncreasingRange();

  void             testComparable      ();

  void             testInterpolate     ();

  void             testFillUsingPreviousValue();

  void             testMergeRanges     ();

  void             testRound           ();

  void             testRintf           ();

  void             testValueInRange    ();

  void             testClockwiseAngle  ();

  static boost::unit_test::test_suite* suite();

};

} // namespace dal

#endif
