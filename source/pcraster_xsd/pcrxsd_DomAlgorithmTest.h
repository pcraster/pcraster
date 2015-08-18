#ifndef INCLUDED_PCRXSD_DOMALGORITHMTEST
#define INCLUDED_PCRXSD_DOMALGORITHMTEST

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
  // DomAlgorithmTest declarations.
}



namespace pcrxsd {

//! This class implements the unit tests for the DomAlgorithm class.
class DomAlgorithmTest
{

private:

public:

                   DomAlgorithmTest           ();

  void             testForEachNode            ();
  void             testForEachElement         ();
  void             testForEachChildElement    ();

  static boost::unit_test::test_suite* suite();

};

} // namespace pcrxsd

#endif
