#ifndef INCLUDED_DAL_DATASPACEADDRESSTEST
#define INCLUDED_DAL_DATASPACEADDRESSTEST



// Library headers.

// PCRaster library headers.

// Module headers.



namespace boost {
  namespace unit_test {
    class test_suite;
  }
}

namespace dal {
  // DataSpaceAddress declarations.
}



namespace dal {



//! This class implements the unit tests for the DataSpaceAddress class.
class DataSpaceAddressTest
{

private:

public:

                   DataSpaceAddressTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             test                ();

  void             testCopy            ();

  static boost::unit_test::test_suite* suite();

};

} // namespace dal

#endif
