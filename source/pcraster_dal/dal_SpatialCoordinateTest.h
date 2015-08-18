#ifndef INCLUDED_DAL_SPATIALCOORDINATETEST
#define INCLUDED_DAL_SPATIALCOORDINATETEST



// External headers.

// Project headers.

// Module headers.



namespace boost {
  namespace unit_test {
    class test_suite;
  }
}

namespace dal {
  // SpatialCoordinateTest declarations.
}



namespace dal {

//! This class implements the unit tests for the SpatialCoordinate class.
class SpatialCoordinateTest
{

private:

public:

                   SpatialCoordinateTest();

  void             test                ();

  void             testCopy            ();

  void             testAssignment      ();

  void             testEquals          ();

  static boost::unit_test::test_suite* suite();

};

} // namespace dal

#endif
