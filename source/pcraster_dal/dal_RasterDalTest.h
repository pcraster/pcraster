#ifndef INCLUDED_DAL_RASTERDALTEST
#define INCLUDED_DAL_RASTERDALTEST



// Library headers.

// PCRaster library headers.

// Module headers.



namespace boost {
  namespace unit_test {
    class test_suite;
  }
}

namespace dal {
  // RasterDal declarations.
}



namespace dal {



//! This class implements the unit tests for the RasterDal class.
class RasterDalTest
{

private:

public:

                   RasterDalTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             testSupportedDrivers();

  void             testESRIASCIIGrid1  ();

  void             testHDF4Image1      ();

  static boost::unit_test::test_suite* suite();

};

} // namespace dal

#endif
