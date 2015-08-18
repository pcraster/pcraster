#ifndef INCLUDED_CALC_MANUALEXAMPLESTEST
#define INCLUDED_CALC_MANUALEXAMPLESTEST



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.
#ifndef INCLUDED_COM_PATHNAME
#include "com_pathname.h"
#define INCLUDED_COM_PATHNAME
#endif

// Module headers.



namespace boost {
  namespace unit_test {
    class test_suite;
  }
}

namespace calc {
  // ManualExamples declarations.
}



namespace calc {



//! This class implements the unit tests for the ManualExamples class.
class ManualExamplesTest
{
  com::PathName  d_startDir;

public:

                   ManualExamplesTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             testAll             ();

  static boost::unit_test::test_suite*     suite               ();

};

} // namespace calc

#endif
