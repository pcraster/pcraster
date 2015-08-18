#ifndef INCLUDED_MLDD_VERTEXITERATORTEST
#define INCLUDED_MLDD_VERTEXITERATORTEST



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
  // VertexIterator declarations.
}



namespace mldd {



//! This class implements the unit tests for the VertexIterator class.
class VertexIteratorTest
{

private:

public:

                   VertexIteratorTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             test                ();

  static boost::unit_test::test_suite* suite();

};

} // namespace mldd

#endif
