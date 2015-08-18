#ifndef INCLUDED_DAL_MATRIXDRIVERTEST
#define INCLUDED_DAL_MATRIXDRIVERTEST



// Library headers.

// PCRaster library headers.

// Module headers.



namespace boost {
  namespace unit_test {
    class test_suite;
  }
}

namespace dal {
  // MatrixDriver declarations.
}



namespace dal {



//! This class implements the unit tests for the MatrixDriver class.
class MatrixDriverTest
{

private:

public:

                   MatrixDriverTest    ();

  void             setUp               ();

  void             tearDown            ();

  static boost::unit_test::test_suite* suite();

};

} // namespace dal

#endif
