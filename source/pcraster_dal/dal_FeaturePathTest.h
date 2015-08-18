#ifndef INCLUDED_DAL_FEATUREPATHTEST
#define INCLUDED_DAL_FEATUREPATHTEST



// External headers.

// Project headers.

// Module headers.



namespace boost {
  namespace unit_test {
    class test_suite;
  }
}

namespace dal {
  // FeaturePathTest declarations.
}



namespace dal {

//! This class implements the unit tests for the FeaturePath class.
class FeaturePathTest
{

private:

public:

                   FeaturePathTest     ();

  void             test                ();

  void             testCompare         ();

  void             testAssignment      ();

  void             testCopy            ();

  static boost::unit_test::test_suite* suite();

};

} // namespace dal

#endif
