#ifndef INCLUDED_DOMINPUTTEST
#define INCLUDED_DOMINPUTTEST



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
  // DOMInput declarations.
}



namespace pcrxsd {



//! This class implements the unit tests for the DOMInput class.
class DOMInputTest
{

private:

public:

                   DOMInputTest        ();

  void             setUp               ();

  void             tearDown            ();

  void             testValidate        ();

  void             testNotWellFormed   ();

  void             testEntityResolver  ();

  static boost::unit_test::test_suite* suite();

};

} // namespace

#endif
