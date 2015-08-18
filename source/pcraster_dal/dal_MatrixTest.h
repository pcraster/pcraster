#ifndef INCLUDED_DAL_MATRIXTEST
#define INCLUDED_DAL_MATRIXTEST



// Library headers.

// PCRaster library headers.

// Module headers.



namespace boost {
  namespace unit_test {
    class test_suite;
  }
}

namespace dal {
  // Matrix declarations.
}



namespace dal {



//! This class implements the unit tests for the Matrix class.
class MatrixTest
{

private:

public:

                   MatrixTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             test                ();

  static boost::unit_test::test_suite* suite();

  void             testExtremes        ();

};

} // namespace dal

#endif
