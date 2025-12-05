#ifndef INCLUDED_MODFLOW_MODFLOWTEST
#define INCLUDED_MODFLOW_MODFLOWTEST

#include "stddefx.h"



namespace boost {
  namespace unit_test {
    class test_suite;
  }
}

namespace mf {
  // ModflowTest declarations.
}



namespace mf {

//! This class implements the unit tests for the Modflow class.
class ModflowTest
{

private:

public:

                   ModflowTest           ();

  void             test                ();

  static boost::unit_test::test_suite* suite();

};

} // namespace mf

#endif
