#ifndef INCLUDED_DEV_TOSTRINGTEST
#define INCLUDED_DEV_TOSTRINGTEST

// External headers.

// Project headers.

// Module headers.



namespace boost {
  namespace unit_test {
    class test_suite;
  }
}

namespace dev {
  // ToStringTest declarations.
}



namespace dev {

//! This class implements the unit tests for the ToString class.
class ToStringTest
{

private:

public:

                   ToStringTest           ();

  void             test                ();

  static boost::unit_test::test_suite* suite();

};

} // namespace dev

#endif
