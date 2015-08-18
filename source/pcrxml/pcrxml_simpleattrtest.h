#ifndef INCLUDED_PCRXML_SIMPLEATTRTEST
#define INCLUDED_PCRXML_SIMPLEATTRTEST



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.

// Module headers.



namespace boost {
  namespace unit_test {
    class test_suite;
  }
}

namespace pcrxml {
  // SimpleAttr declarations.
}



namespace pcrxml {



//! This class implements the unit tests for the SimpleAttr class.
class SimpleAttrTest
{

public:

                   SimpleAttrTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             testIt              ();

  static boost::unit_test::test_suite*   suite               ();

};

} // namespace pcrxml

#endif
