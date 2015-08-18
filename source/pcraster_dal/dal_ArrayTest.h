#ifndef INCLUDED_DAL_ARRAYTEST
#define INCLUDED_DAL_ARRAYTEST



// Library headers.

// PCRaster library headers.

// Module headers.



namespace boost {
  namespace unit_test {
    class test_suite;
  }
}

namespace dal {
  // Array declarations.
}



namespace dal {



//! This class implements the unit tests for the Array class.
class ArrayTest
{

private:

public:

                   ArrayTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             testConstructor     ();

  void             testIndexOf         ();

  static boost::unit_test::test_suite* suite();

};

} // namespace dal

#endif
