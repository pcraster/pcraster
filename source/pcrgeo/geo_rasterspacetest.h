#ifndef INCLUDED_GEO_RASTERSPACETEST
#define INCLUDED_GEO_RASTERSPACETEST



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_GEO_RASTERSPACE
#include "geo_rasterspace.h"
#define INCLUDED_GEO_RASTERSPACE
#endif



namespace boost {
  namespace unit_test {
    class test_suite;
  }
}



namespace geo {



/*!
  \class RasterSpaceTest
  \brief This class implements the unit tests for the RasterSpace class.
*/
//       1         2         3         4         5         6         7         8
class RasterSpaceTest
{

private:

  RasterSpace *    d_rs1;

  RasterSpace *    d_rs2;

public:

  static boost::unit_test::test_suite* suite();

  //! Constructor.
                   RasterSpaceTest     ();

  void             setUp               ();

  void             tearDown            ();

  void             testEquality        ();

  void             testNrRows          ();

  void             testNrCols          ();

  void             testNrCells         ();

  void             testQuadrant        ();

  void             testIO              ();

};



//------------------------------------------------------------------------------
// INLINE FUNCIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace geo

#endif
