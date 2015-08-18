#ifndef INCLUDED_DAL_TYPESTEST
#define INCLUDED_DAL_TYPESTEST



// Library headers.

// PCRaster library headers.

// Module headers.



namespace boost {
  namespace unit_test {
    class test_suite;
  }
}

namespace dal {
  // Types declarations.
}



namespace dal {



//! This class implements the unit tests for the Types class.
class TypesTest
{

private:

public:

                   TypesTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             testIdOfSmallestType();

  void             testIdOfLargestType ();

  static boost::unit_test::test_suite* suite();

};

} // namespace dal

#endif
