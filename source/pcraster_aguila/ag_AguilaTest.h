#ifndef INCLUDED_AG_AGUILATEST
#define INCLUDED_AG_AGUILATEST



// External headers.

// Project headers.

// Module headers.



namespace boost {
  namespace unit_test {
    class test_suite;
  }
}

namespace ag {
  // AguilaTest declarations.
}



namespace ag {

//! This class implements the unit tests for the Aguila class.
class AguilaTest
{

private:

public:

                   AguilaTest          ();

  void             testGui             ();

  static boost::unit_test::test_suite* suite();

};

} // namespace ag

#endif
