#ifndef INCLUDED_AG_COLOURSELECTORTEST
#define INCLUDED_AG_COLOURSELECTORTEST



// External headers.

// Project headers.

// Module headers.



namespace boost {
  namespace unit_test {
    class test_suite;
  }
}

namespace ag {
  // ColourSelectorTest declarations.
}



namespace ag {

//! This class implements the unit tests for the ColourSelector class.
class ColourSelectorTest
{

private:

public:

                   ColourSelectorTest  ();

  void             test                ();

  static boost::unit_test::test_suite* suite();

};

} // namespace ag

#endif
