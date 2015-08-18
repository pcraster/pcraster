#ifndef INCLUDED_GEO_SCANCONVERSIONTEST
#define INCLUDED_GEO_SCANCONVERSIONTEST



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_GEO_SCANCONVERSION
#include "geo_scanconversion.h"
#define INCLUDED_GEO_SCANCONVERSION
#endif



namespace boost {
  namespace unit_test {
    class test_suite;
  }
}

namespace geo {
  // ScanConversion declarations.
}



namespace geo {



//! This class implements the unit tests for the ScanConversion class.
class ScanConversionTest
{

  template<class Integral>
  void             testCirclePoints    (
                                  const RememberPoints<Integral>& points,
                                  const std::pair<Integral, Integral>& point);

public:

                   ScanConversionTest  ();

  void             setUp               ();

  void             tearDown            ();

  void             testMidpointLine    ();

  void             testMidpointCircleNrPoints();

  void             testMidpointCircle  ();

  static boost::unit_test::test_suite* suite();

};

} // namespace geo

#endif
