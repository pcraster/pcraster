#ifndef INCLUDED_DEV_MATHUTILSTEST
#define INCLUDED_DEV_MATHUTILSTEST

// External headers.

// Project headers.

// Module headers.



namespace boost {
  namespace unit_test {
    class test_suite;
  }
}

namespace dev {
  // MathUtilsTest declarations.
}



namespace dev {

//! This class implements the unit tests for the MathUtils class.
class MathUtilsTest
{

private:

public:

                   MathUtilsTest           ();

  void             test                ();

  static boost::unit_test::test_suite* suite();

};

} // namespace dev

#endif
