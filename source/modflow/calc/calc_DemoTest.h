#ifndef INCLUDED_CALC_DEMOTEST
#define INCLUDED_CALC_DEMOTEST



// External headers.

// Project headers.

// Module headers.



namespace boost {
  namespace unit_test {
    class test_suite;
  }
}

namespace calc {
  // DemoTest declarations.
}



namespace calc {

//! This class implements the unit tests for the Demo class.
class DemoTest
{

private:

public:

                   DemoTest            ();

  void             test_demo           ();

  void             test_bcf2ss         ();

  void             test_python_scripts ();

  static boost::unit_test::test_suite* suite();

};

} // namespace calc

#endif
