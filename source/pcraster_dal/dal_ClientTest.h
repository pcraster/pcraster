#ifndef INCLUDED_DAL_CLIENTTEST
#define INCLUDED_DAL_CLIENTTEST



// External headers.

// Project headers.

// Module headers.



namespace boost {
  namespace unit_test {
    class test_suite;
  }
}

namespace dal {
  // ClientTest declarations.
}



namespace dal {

//! This class implements the unit tests for the Client class.
class ClientTest
{

private:

public:

                   ClientTest          ();

  void             test                ();

  static boost::unit_test::test_suite* suite();

};

} // namespace dal

#endif
