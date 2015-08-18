#ifndef INCLUDED_COM_LABELEDPROGRESSTRACKERTEST
#define INCLUDED_COM_LABELEDPROGRESSTRACKERTEST



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
  // LabeledProgressTracker declarations.
}



namespace com {



//! This class implements the unit tests for the LabeledProgressTracker class.
class LabeledProgressTrackerTest
{

private:

public:

                   LabeledProgressTrackerTest();

  void             setUp               ();

  void             tearDown            ();

  void             test                ();

  static boost::unit_test::test_suite* suite();

};

} // namespace com

#endif
