#ifndef INCLUDED_PCRXML_DOMTEST
#define INCLUDED_PCRXML_DOMTEST



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
  // Dom declarations.
}



namespace pcrxml {



//! This class implements the unit tests for the Dom class.
class DomTest
{

public:

                   DomTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             testFirstMatchByTagName();
  void             testChildrenByTagName();

  void             testChangeAttrName  ();
  void             testTextOnlyContents();
  void             testDomEquality     ();

  static boost::unit_test::test_suite*    suite               ();

};

} // namespace pcrxml

#endif
