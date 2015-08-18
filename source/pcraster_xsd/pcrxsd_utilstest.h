#ifndef INCLUDED_UTILSTEST
#define INCLUDED_UTILSTEST



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
  // Utils declarations.
}



namespace pcrxsd {



//! This class implements the unit tests for the Utils class.
class UtilsTest
{

private:

public:

                   UtilsTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             testContentsIsXMLOrPCRasterFileFormat();

  static boost::unit_test::test_suite* suite();

};

} // namespace

#endif
