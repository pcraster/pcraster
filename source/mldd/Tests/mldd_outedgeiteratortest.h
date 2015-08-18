#ifndef INCLUDED_MLDD_OUTEDGEITERATORTEST
#define INCLUDED_MLDD_OUTEDGEITERATORTEST



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


namespace mldd {
  // OutEdgeIterator declarations.
}



namespace mldd {



//! This class implements the unit tests for the OutEdgeIterator class.
class OutEdgeIteratorTest
{

public:

                   OutEdgeIteratorTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             testCtor            ();

  static boost::unit_test::test_suite*     suite               ();

};

} // namespace mldd

#endif
