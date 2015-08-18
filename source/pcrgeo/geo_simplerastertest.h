#ifndef INCLUDED_GEO_SIMPLERASTERTEST
#define INCLUDED_GEO_SIMPLERASTERTEST



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_GEO_SIMPLERASTER
#include "geo_simpleraster.h"
#define INCLUDED_GEO_SIMPLERASTER
#endif



namespace boost {
  namespace unit_test {
    class test_suite;
  }
}



namespace geo {



//! This class implements the unit tests for the SimpleRaster class.
class SimpleRasterTest
{

private:

  SimpleRaster<int>* d_raster1;
  SimpleRaster<int>* d_raster2;
  SimpleRaster<int>* d_raster3;

public:

                   SimpleRasterTest    ();

  void             setUp               ();

  void             tearDown            ();

  void             testProperties      ();

  void             testContents        ();

  void             testAssignment      ();

  static boost::unit_test::test_suite* suite();

};

} // namespace geo

#endif
