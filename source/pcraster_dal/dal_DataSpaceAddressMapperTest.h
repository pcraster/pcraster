#ifndef INCLUDED_DAL_DATASPACEADDRESSMAPPERTEST
#define INCLUDED_DAL_DATASPACEADDRESSMAPPERTEST



// Library headers.

// PCRaster library headers.

// Module headers.



namespace boost {
  namespace unit_test {
    class test_suite;
  }
}

namespace dal {
  // DataSpaceAddressMapper declarations.
}



namespace dal {



//! This class implements the unit tests for the DataSpaceAddressMapper class.
class DataSpaceAddressMapperTest
{

private:

public:

                   DataSpaceAddressMapperTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             test                ();

  static boost::unit_test::test_suite* suite();

};

} // namespace dal

#endif
