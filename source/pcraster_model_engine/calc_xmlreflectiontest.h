#ifndef INCLUDED_CALC_XMLREFLECTIONTEST
#define INCLUDED_CALC_XMLREFLECTIONTEST



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

namespace calc {
  // XMLReflection declarations.
}



namespace calc {



//! This class implements the unit tests for the XMLReflection class.
class XMLReflectionTest
{

private:

public:

                   XMLReflectionTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             testXMLReflection   ();
  void             testEsriXMLReflection();

  static boost::unit_test::test_suite* suite();

};

} // namespace calc

#endif
