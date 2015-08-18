#ifndef INCLUDED_COM_PROGRESSBARTEST
#define INCLUDED_COM_PROGRESSBARTEST



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
  // ProgressBar declarations.
}



namespace com {



//! This class implements the unit tests for the ProgressBar class.
class ProgressBarTest
{

private:

public:

                   ProgressBarTest     ();

  void             setUp               ();

  void             tearDown            ();

  void             test                ();

  static boost::unit_test::test_suite* suite();

};

} // namespace com

#endif
