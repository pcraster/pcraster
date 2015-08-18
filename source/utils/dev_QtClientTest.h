#ifndef INCLUDED_DEV_QTCLIENTTEST
#define INCLUDED_DEV_QTCLIENTTEST



// External headers.

// Project headers.

// Module headers.



namespace boost {
  namespace unit_test {
    class test_suite;
  }
}

namespace dev {
  // QtClientTest declarations.
}



namespace dev {

//! This class implements the unit tests for the QtClient class.
class QtClientTest
{

private:

public:

                   QtClientTest        ();

  void             test                ();

  static boost::unit_test::test_suite* suite();

};

} // namespace dev

#endif
