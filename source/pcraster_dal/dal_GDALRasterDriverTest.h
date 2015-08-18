#ifndef INCLUDED_DAL_GDALRASTERDRIVERTEST
#define INCLUDED_DAL_GDALRASTERDRIVERTEST



// Library headers.

// PCRaster library headers.

// Module headers.



namespace boost {
  namespace unit_test {
    class test_suite;
  }
}

namespace dal {
  // GDALRasterDriver declarations.
}



namespace dal {



//! This class implements the unit tests for the GDALRasterDriver class.
class GDALRasterDriverTest
{

private:

public:

                   GDALRasterDriverTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             testDescription     ();

  void             testUnexisting      ();

  void             testEmpty           ();

  void             testPCRaster        ();

  // void             testHdf4Image       ();

  // void             testHdf5            ();

  void             testESRIASCIIGrid1  ();

  void             testGeoTiff         ();

  void             testWrite           ();

  void             testDefaultExtension();

  void             testRectangularCells();

  static boost::unit_test::test_suite* suite();

};

} // namespace dal

#endif
