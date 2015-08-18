#ifndef INCLUDED_COM_PATHINFOTEST
#define INCLUDED_COM_PATHINFOTEST

#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_COM_PATHINFO
#include "com_pathinfo.h"
#define INCLUDED_COM_PATHINFO
#endif


namespace boost {
  namespace unit_test {
    class test_suite;
  }
}



namespace com {



/*!
  \class PathInfoTest
  \brief This class implements the unit tests for the PathInfo class.
*/
//       1         2         3         4         5         6         7         8
class PathInfoTest
{

private:

public:

                   PathInfoTest        ();

  void             setUp               ();

  void             tearDown            ();

  void             testTempDirectoryName();

  void             testExists          ();

  void             testIsDirectory     ();

  void             testIsFile          ();

  void             testIsReadable      ();

  void             testIsWritable      ();

  void             testTestCaseSensitiveName();

  void             testChangeWorkingDirectory();

  static boost::unit_test::test_suite* suite();

};

} // namespace com

#endif
