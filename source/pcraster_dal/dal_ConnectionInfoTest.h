#ifndef INCLUDED_DAL_CONNECTIONINFOTEST
#define INCLUDED_DAL_CONNECTIONINFOTEST



// Library headers.

// PCRaster library headers.

// Module headers.



namespace boost {
  namespace unit_test {
    class test_suite;
  }
}

namespace dal {
  // ConnectionInfo declarations.
}



namespace dal {



//! This class implements the unit tests for the ConnectionInfo class.
class ConnectionInfoTest
{

private:

public:

                   ConnectionInfoTest  ();

  void             setUp               ();

  void             tearDown            ();

  void             test                ();

  static boost::unit_test::test_suite* suite();

};

} // namespace dal

#endif
