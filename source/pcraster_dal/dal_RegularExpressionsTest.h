#ifndef INCLUDED_DAL_REGULAREXPRESSIONSTEST
#define INCLUDED_DAL_REGULAREXPRESSIONSTEST



// External headers.

// Project headers.

// Module headers.



namespace boost {
  namespace unit_test {
    class test_suite;
  }
}

namespace dal {
  // RegularExpressionsTest declarations.
}



namespace dal {

//! This class implements the unit tests for the RegularExpressions class.
class RegularExpressionsTest
{

private:

public:

                   RegularExpressionsTest();

  void             testQuantileOfRasterRegex();

  static boost::unit_test::test_suite* suite();

};

} // namespace dal

#endif
