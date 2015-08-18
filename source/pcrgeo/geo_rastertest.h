#ifndef INCLUDED_GEO_RASTERTEST
#define INCLUDED_GEO_RASTERTEST



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif



namespace boost {
  namespace unit_test {
    class test_suite;
  }
}



namespace geo {



/*!
  \class RasterTest
  \brief This class implements the unit tests for the Raster class.
*/
//       1         2         3         4         5         6         7         8
class RasterTest
{
private:

public:

  static boost::unit_test::test_suite* suite();

  //! Constructor.
                   RasterTest          ();

  void             setUp               ();

  void             tearDown            ();

  //! Tests if a raster returns the right cell, given a row and column number.
  void             testCell            ();

};

} // namespace geo

#endif
