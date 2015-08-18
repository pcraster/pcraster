#ifndef INCLUDED_PCRXSD_XSDTEST
#define INCLUDED_PCRXSD_XSDTEST



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
  // Xsd declarations.
}



namespace pcrxsd {



//! This class implements the unit tests for the Xsd class.
class XsdTest
{

private:

public:

                   XsdTest             ();

  void             setUp               ();

  void             tearDown            ();

  void             testValidation      ();
  void             testXML2Class       ();
  void             testClass2XML       ();
  void             testNoSchema        ();

  static boost::unit_test::test_suite* suite();

};

} // namespace pcrxsd

#endif
