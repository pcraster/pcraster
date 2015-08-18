#ifndef INCLUDED_DAL_NETCDFRASTERDRIVERTEST
#define INCLUDED_DAL_NETCDFRASTERDRIVERTEST



// Library headers.

// PCRaster library headers.

// Module headers.



namespace boost {
  namespace unit_test {
    class test_suite;
  }
}

namespace dal {
  // NetCDFRasterDriverTest declarations.
}



namespace dal {

//! This class implements the unit tests for the NetCDFRasterDriver class.
class NetCDFRasterDriverTest
{

private:

public:

                   NetCDFRasterDriverTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             testDefaultExtension();

  void             testEmptyDataSpace  ();

  static boost::unit_test::test_suite* suite();

};

} // namespace dal

#endif
