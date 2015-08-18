#ifndef INCLUDED_AG_AGUILAPROGRAMOPTIONSTEST
#define INCLUDED_AG_AGUILAPROGRAMOPTIONSTEST



// Library headers.

// PCRaster library headers.

// Module headers.



namespace boost {
  namespace unit_test {
    class test_suite;
  }
}

namespace ag {
  // AguilaProgramOptions declarations.
}



namespace ag {



//! This class implements the unit tests for the AguilaProgramOptions class.
class AguilaProgramOptionsTest
{

private:

public:

                   AguilaProgramOptionsTest();

  void             testBoostOptions    ();

  void             testBoostOptions2XML();

  void             testViewPlusSyntaxToViewCtor();

  void             testStacknameFix    ();

  void             testDrapeSyntax     ();

  void             testMultipleViews   ();

  static boost::unit_test::test_suite* suite();

};

} // namespace ag

#endif
