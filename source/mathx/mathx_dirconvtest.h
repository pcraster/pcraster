#ifndef INCLUDED_MATHX_DIRCONVTEST
#define INCLUDED_MATHX_DIRCONVTEST

#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

namespace boost {
  namespace unit_test {
    class test_suite;
  }
}

namespace mathx {


class DirConvTest
{

private:

public:

                   DirConvTest         ();

  void             setUp               ();

  void             tearDown            ();

  void             testScaleDeg        ();

  static boost::unit_test::test_suite* suite();

};

} // namespace calc

#endif
