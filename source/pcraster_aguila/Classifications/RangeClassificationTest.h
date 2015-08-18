#ifndef INCLUDED_RANGECLASSIFICATIONTEST
#define INCLUDED_RANGECLASSIFICATIONTEST



// External headers.

// Project headers.

// Module headers.



namespace boost {
  namespace unit_test {
    class test_suite;
  }
}

namespace ag {
  // RangeClassificationTest declarations.
}



namespace ag {

//! This class implements the unit tests for the RangeClassification class.
class RangeClassificationTest
{

private:

public:

                   RangeClassificationTest();

  void             testDefaultConstructor ();

  void             testLinear             ();

  void             testLog10              ();

  static boost::unit_test::test_suite* suite();

};

} // namespace ag

#endif
