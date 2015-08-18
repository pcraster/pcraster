#ifndef INCLUDED_COM_INTABLESTREAMTEST
#define INCLUDED_COM_INTABLESTREAMTEST



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

namespace com {
  // InTableStream declarations.
}



namespace com {



//! This class implements the unit tests for the InTableStream class.
class InTableStreamTest
{

public:

                   InTableStreamTest   ();

  void             setUp               ();

  void             tearDown            ();

  void             testRead            ();

  void             testRead2           ();

  void             testFormatErrors    ();

  static boost::unit_test::test_suite* suite();

};

} // namespace com

#endif
