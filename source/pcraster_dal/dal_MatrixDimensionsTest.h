#ifndef INCLUDED_DAL_MATRIXDIMENSIONSTEST
#define INCLUDED_DAL_MATRIXDIMENSIONSTEST



// External headers.

// Project headers.

// Module headers.



namespace boost {
  namespace unit_test {
    class test_suite;
  }
}

namespace dal {
  // MatrixDimensionsTest declarations.
}



namespace dal {

//! This class implements the unit tests for the MatrixDimensions class.
class MatrixDimensionsTest
{

private:

public:

                   MatrixDimensionsTest();

  void             test                ();

  static boost::unit_test::test_suite* suite();

};

} // namespace dal

#endif
