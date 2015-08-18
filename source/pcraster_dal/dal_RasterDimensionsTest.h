#ifndef INCLUDED_DAL_RASTERDIMENSIONSTEST
#define INCLUDED_DAL_RASTERDIMENSIONSTEST

// External headers.

// Project headers.

// Module headers.



namespace boost {
  namespace unit_test {
    class test_suite;
  }
}

namespace dal {
  // RasterDimensionsTest declarations.
}



namespace dal {

//! This class implements the unit tests for the RasterDimensions class.
class RasterDimensionsTest
{

private:

public:

                   RasterDimensionsTest();

  void             test                ();

  void             testAssignment      ();

  void             testIndex           ();

  void             testCoordinates     ();

  void             testAreaDimensions  ();

  void             testOverlap         ();

  static boost::unit_test::test_suite* suite();

};

} // namespace dal

#endif
