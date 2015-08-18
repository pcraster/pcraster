#ifndef INCLUDED_DATAOBJECTBASETEST
#define INCLUDED_DATAOBJECTBASETEST



// External headers.

// Project headers.

// Module headers.



namespace boost {
  namespace unit_test {
    class test_suite;
  }
}

namespace ag {
  // DataObjectBaseTest declarations.
}



namespace ag {

//! This class implements the unit tests for the DataObjectBase class.
class DataObjectBaseTest
{

private:

public:

                   DataObjectBaseTest  ();

  void             test                ();

  static boost::unit_test::test_suite* suite();

};

} // namespace ag

#endif
