#ifndef INCLUDED_PCRXML_BINDOUBLELETEST
#define INCLUDED_PCRXML_BINDOUBLELETEST



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
  // BinDoubleLE declarations.
}



namespace pcrxml {



//! This class implements the unit tests for the BinDoubleLE class.
class BinDoubleLETest
{

public:

                   BinDoubleLETest           ();

  void             setUp               ();

  void             tearDown            ();

  void             testEncoding        ();

  static boost::unit_test::test_suite*    suite               ();

};

} // namespace pcrxml

#endif
