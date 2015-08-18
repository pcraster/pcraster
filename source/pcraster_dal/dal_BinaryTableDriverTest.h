#ifndef INCLUDED_DAL_BINARYTABLEDRIVERTEST
#define INCLUDED_DAL_BINARYTABLEDRIVERTEST



// Library headers.

// PCRaster library headers.

// Module headers.



namespace boost {
  namespace unit_test {
    class test_suite;
  }
}

namespace dal {
  // BinaryTableDriver declarations.
}



namespace dal {



//! This class implements the unit tests for the BinaryTableDriver class.
class BinaryTableDriverTest
{

private:

public:

                   BinaryTableDriverTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             test                ();

  static boost::unit_test::test_suite* suite();

};

} // namespace dal

#endif
