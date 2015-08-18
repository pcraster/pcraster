#ifndef INCLUDED_DAL_VECTORDRIVERTEST
#define INCLUDED_DAL_VECTORDRIVERTEST



// External headers.

// Project headers.

// Module headers.



namespace boost {
  namespace unit_test {
    class test_suite;
  }
}

namespace dal {
  // VectorDriverTest declarations.
}



namespace dal {

//! This class implements the unit tests for the VectorDriver class.
class VectorDriverTest
{

private:

public:

                   VectorDriverTest    ();

  void             testExists          ();

  void             testOpen            ();

  void             testDataSpace       ();

  void             testRead            ();

  void             testExtremes        ();

  static boost::unit_test::test_suite* suite();

};

} // namespace dal

#endif
