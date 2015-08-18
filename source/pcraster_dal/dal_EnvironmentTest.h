#ifndef INCLUDED_DAL_ENVIRONMENTTEST
#define INCLUDED_DAL_ENVIRONMENTTEST



// External headers.

// Project headers.

// Module headers.



namespace boost {
  namespace unit_test {
    class test_suite;
  }
}

namespace dal {
  // EnvironmentTest declarations.
}



namespace dal {

//! This class implements the unit tests for the Environment class.
class EnvironmentTest
{

private:

public:

                   EnvironmentTest     ();

  void             testDalFormats      ();

  void             testGdalData        ();

  static boost::unit_test::test_suite* suite();

};

} // namespace dal

#endif
