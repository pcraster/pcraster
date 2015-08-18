#ifndef INCLUDED_DATAMANAGERTEST
#define INCLUDED_DATAMANAGERTEST



// External headers.

// Project headers.

// Module headers.



namespace boost {
  namespace unit_test {
    class test_suite;
  }
}

namespace ag {
  // DataManagerTest declarations.
}



namespace ag {

//! This class implements the unit tests for the DataManager class.
class DataManagerTest
{

private:

public:

                   DataManagerTest     ();

  void             test                ();

  static boost::unit_test::test_suite* suite();

};

} // namespace ag

#endif
