#ifndef INCLUDED_DEV_UTILSTEST
#define INCLUDED_DEV_UTILSTEST



// External headers.

// Project headers.

// Module headers.



namespace boost {
  namespace unit_test {
    class test_suite;
  }
}

namespace dev {
  // UtilsTest declarations.
}



namespace dev {

//! This class implements the unit tests for the Utils class.
class UtilsTest
{

private:

public:

                   UtilsTest           ();

  void             testUnique          ();

  void             testEnvironmentVariables();

  static boost::unit_test::test_suite* suite();

};

} // namespace dev

#endif
