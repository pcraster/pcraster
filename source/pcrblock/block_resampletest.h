#ifndef INCLUDED_BLOCK_RESAMPLETEST
#define INCLUDED_BLOCK_RESAMPLETEST



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

namespace block {
  // Resample declarations.
}



namespace block {



//! This class implements the unit tests for the Resample class.
class ResampleTest
{

private:

public:

                   ResampleTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             testResampleBlock   ();

  void             testResampleBlockData();

  static boost::unit_test::test_suite* suite();

};

} // namespace block

#endif
