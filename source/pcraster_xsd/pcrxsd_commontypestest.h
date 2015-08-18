#ifndef INCLUDED_PCRXSD_COMMONTYPESTEST
#define INCLUDED_PCRXSD_COMMONTYPESTEST



// #ifndef INCLUDED_STDDEFX
// #include "stddefx.h"
// #define INCLUDED_STDDEFX
// #endif

// Library headers.

// PCRaster library headers.

// Module headers.



namespace boost {
  namespace unit_test {
    class test_suite;
  }
}

namespace pcrxsd {
  // CommonTypes declarations.
}



namespace pcrxsd {



//! This class implements the unit tests for the CommonTypes class.
class CommonTypesTest
{

private:

public:

                   CommonTypesTest     ();

  void             setUp               ();

  void             tearDown            ();

  void             testToBoostPosixTime();
  void             testTimeDurationAssumption();
  void             testTimeDuration    ();

  static boost::unit_test::test_suite* suite();

};

} // namespace pcrxsd

#endif
