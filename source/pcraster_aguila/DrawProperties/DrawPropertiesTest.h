#ifndef INCLUDED_DRAWPROPERTIESTEST
#define INCLUDED_DRAWPROPERTIESTEST



// External headers.

// Project headers.

// Module headers.



namespace boost {
  namespace unit_test {
    class test_suite;
  }
}

namespace ag {
  // DrawPropertiesTest declarations.
}



namespace ag {

//! This class implements the unit tests for the DrawProperties class.
class DrawPropertiesTest
{

private:

public:

                   DrawPropertiesTest  ();

  void             test                ();

  static boost::unit_test::test_suite* suite();

};

} // namespace ag

#endif
