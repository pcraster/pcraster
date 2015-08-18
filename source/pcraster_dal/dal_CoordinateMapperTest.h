#ifndef INCLUDED_DAL_COORDINATEMAPPERTEST
#define INCLUDED_DAL_COORDINATEMAPPERTEST



// Library headers.

// PCRaster library headers.

// Module headers.



namespace boost {
  namespace unit_test {
    class test_suite;
  }
}

namespace dal {
  // CoordinateMapper declarations.
}



namespace dal {



//! This class implements the unit tests for the CoordinateMapper class.
class CoordinateMapperTest
{

private:

public:

                   CoordinateMapperTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             test                ();

  static boost::unit_test::test_suite* suite();

};

} // namespace dal

#endif
