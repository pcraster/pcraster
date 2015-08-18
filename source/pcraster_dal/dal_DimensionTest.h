#ifndef INCLUDED_DAL_DIMENSIONTEST
#define INCLUDED_DAL_DIMENSIONTEST



// Library headers.

// PCRaster library headers.

// Module headers.



namespace boost {
  namespace unit_test {
    class test_suite;
  }
}

namespace dal {
  // Dimension declarations.
}



namespace dal {



//! This class implements the unit tests for the Dimension class.
class DimensionTest
{

private:

public:

                   DimensionTest       ();

  void             setUp               ();

  void             tearDown            ();

  void             test                ();

  void             testContainsValueInRange();

  void             testClamp           ();

  void             testIndexOfValueOf  ();

  void             testMerge           ();

  void             testIntersect       ();

  void             testNrCoordinates   ();

  void             testIsWide          ();

  static boost::unit_test::test_suite* suite();

};

} // namespace dal

#endif
