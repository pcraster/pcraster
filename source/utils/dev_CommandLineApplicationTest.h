#ifndef INCLUDED_DEV_COMMANDLINEAPPLICATIONTEST
#define INCLUDED_DEV_COMMANDLINEAPPLICATIONTEST

// External headers.

// Project headers.

// Module headers.



namespace boost {
  namespace unit_test {
    class test_suite;
  }
}

namespace dev {
  // CommandLineApplicationTest declarations.
}



namespace dev {

//! This class implements the unit tests for the CommandLineApplication class.
class CommandLineApplicationTest
{

private:

public:

                   CommandLineApplicationTest();

  void             test                ();

  static boost::unit_test::test_suite* suite();

};

} // namespace dev

#endif
