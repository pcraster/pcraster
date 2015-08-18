#ifndef INCLUDED_DAL_TIMESTEPCOORDINATEMAPPER
#define INCLUDED_DAL_TIMESTEPCOORDINATEMAPPER



// Library headers.

// PCRaster library headers.

// Module headers.



namespace boost {
  namespace unit_test {
    class test_suite;
  }
}

namespace dal {
  // TimeStepCoordinateMapper declarations.
}



namespace dal {



//! This class implements the unit tests for the TimeStepCoordinateMapper class.
class TimeStepCoordinateMapperTest
{

private:

public:

                   TimeStepCoordinateMapperTest();

  void             setUp               ();

  void             tearDown            ();

  void             test                ();

  static boost::unit_test::test_suite* suite();

};

} // namespace dal

#endif
