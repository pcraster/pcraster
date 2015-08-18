#ifndef INCLUDED_AG_CLASSIFIERTEST
#define INCLUDED_AG_CLASSIFIERTEST



// External headers.

// Project headers.

// Module headers.



namespace boost {
  namespace unit_test {
    class test_suite;
  }
}

namespace ag {
  // ClassifierTest declarations.
}



namespace ag {

//! This class implements the unit tests for the Classifier class.
class ClassifierTest
{

private:

public:

                   ClassifierTest      ();

  void             test                ();

  static boost::unit_test::test_suite* suite();

};

} // namespace ag

#endif
