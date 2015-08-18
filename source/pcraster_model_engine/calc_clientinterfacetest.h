#ifndef INCLUDED_CALC_CLIENTINTERFACETEST
#define INCLUDED_CALC_CLIENTINTERFACETEST



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
  // ClientInterface declarations.
}



namespace calc {



//! tests the pcrcalc C-API (pcrcalc.cc)
class ClientInterfaceTest
{

public:

                   ClientInterfaceTest ();

  void             testCapi            ();

  void             testIOMemoryStatic  ();
  void             testIOMemoryDynamic ();
  void             testIOMemoryTimeoutput ();

  void             testBil             ();

  void             testFromString      ();

  void             testXML             ();
  void             testXMLHabitat      ();
  void             testXMLStatistics   ();
  void             testXMLSettings     ();

  static boost::unit_test::test_suite*     suite               ();

};

class APIInitTest
{

public:

                   APIInitTest        ();

  void             setUp               ();

  void             tearDown            ();

  void             testInit            ();

  static boost::unit_test::test_suite*     suite               ();

};

} // namespace calc

#endif
