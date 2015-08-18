#ifndef INCLUDED_DAL_HDF5RASTERDRIVERTEST
#define INCLUDED_DAL_HDF5RASTERDRIVERTEST



// Library headers.

// PCRaster library headers.

// Module headers.



namespace boost {
  namespace unit_test {
    class test_suite;
  }
}

namespace dal {
  // HDF5RasterDriverTest declarations.
}



namespace dal {

//! This class implements the unit tests for the HDF5RasterDriver class.
class HDF5RasterDriverTest
{

private:

public:

                   HDF5RasterDriverTest();

  void             setUp               ();

  void             tearDown            ();

  void             testEmptyDataSpace  ();

  static boost::unit_test::test_suite* suite();

};

} // namespace dal

#endif
