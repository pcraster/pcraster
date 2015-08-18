#ifndef INCLUDED_PCRXML_DOMALGORITHMTEST
#define INCLUDED_PCRXML_DOMALGORITHMTEST



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
  // DomAlgorithm declarations.
}



namespace pcrxml {



//! This class implements the unit tests for the DomAlgorithm class.
class DomAlgorithmTest
{

public:

                   DomAlgorithmTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             testForEachNode     ();

  void             testForEachElement  ();

  void             testForEachChildElement  ();

  static boost::unit_test::test_suite*    suite               ();

};

} // namespace pcrxml

#endif
