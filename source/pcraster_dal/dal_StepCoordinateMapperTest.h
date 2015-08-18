#ifndef INCLUDED_DAL_STEPCOORDINATEMAPPERTEST
#define INCLUDED_DAL_STEPCOORDINATEMAPPERTEST



// Library headers.

// PCRaster library headers.

// Module headers.



namespace boost {
  namespace unit_test {
    class test_suite;
  }
}

namespace dal {
  // StepCoordinateMapper declarations.
}



namespace dal {



//! This class implements the unit tests for the StepCoordinateMapper class.
class StepCoordinateMapperTest
{

private:

public:

                   StepCoordinateMapperTest();

  void             setUp               ();

  void             tearDown            ();

  void             test                ();

  static boost::unit_test::test_suite* suite();

};

} // namespace dal

#endif
