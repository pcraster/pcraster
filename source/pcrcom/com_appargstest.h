#ifndef INCLUDED_COM_APPARGSTEST
#define INCLUDED_COM_APPARGSTEST

#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

namespace boost {
  namespace unit_test {
    class test_suite;
  }
}

namespace com {



/*!
  \class AppArgsTest
  \brief This class implements the unit tests for the StrLib class.
*/
//       1         2         3         4         5         6         7         8
class AppArgsTest
{

private:

public:

                   AppArgsTest         ();

  void             setUp               ();

  void             tearDown            ();

  void             testArgvArgc        ();

  static boost::unit_test::test_suite* suite();

};

} // namespace com

#endif
