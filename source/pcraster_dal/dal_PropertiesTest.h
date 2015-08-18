#ifndef INCLUDED_DAL_PROPERTIESTEST
#define INCLUDED_DAL_PROPERTIESTEST



// Library headers.

// PCRaster library headers.

// Module headers.



namespace boost {
  namespace unit_test {
    class test_suite;
  }
}

namespace dal {
  // Properties declarations.
}



namespace dal {



//! This class implements the unit tests for the Properties class.
class PropertiesTest
{

private:

public:

                   PropertiesTest      ();

  void             setUp               ();

  void             tearDown            ();

  void             test                ();

  static boost::unit_test::test_suite* suite();

};

} // namespace dal

#endif
