#ifndef INCLUDED_COM_ALGORITHMTEST
#define INCLUDED_COM_ALGORITHMTEST



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif

// PCRaster library headers.

// Module headers.



namespace boost {
  namespace unit_test {
    class test_suite;
  }
}

namespace com {
  // Algorithm declarations.
}



namespace com {



//! This class implements the unit tests for the Algorithm class.
class AlgorithmTest
{

public:

                   AlgorithmTest       ();

  void             setUp               ();

  void             tearDown            ();

  void             testHasElement      ();
  void             testSeq0123etc      ();
  void             testFindValue       ();

  static boost::unit_test::test_suite* suite();

};

} // namespace com

#endif
