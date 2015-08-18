#ifndef INCLUDED_DAL_DATASPACETEST
#define INCLUDED_DAL_DATASPACETEST



// Library headers.

// PCRaster library headers.

// Module headers.



namespace boost {
  namespace unit_test {
    class test_suite;
  }
}

namespace dal {
  // DataSpace declarations.
}



namespace dal {



//! This class implements the unit tests for the DataSpace class.
class DataSpaceTest
{

private:

public:

                   DataSpaceTest       ();

  void             setUp               ();

  void             tearDown            ();

  void             test                ();

  void             testMerge           ();

  void             testIntersect       ();

  void             testTrim            ();

  void             testInitialiseInvalidCoordinates();

  void             testEquals          ();

  void             testReplaceDimension();

  void             testContains        ();

  static boost::unit_test::test_suite* suite();

};

} // namespace dal

#endif
