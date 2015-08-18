#ifndef INCLUDED_COM_PATHNAMETEST
#define INCLUDED_COM_PATHNAMETEST

#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_COM_PATHNAME
#include "com_pathname.h"
#define INCLUDED_COM_PATHNAME
#endif



namespace boost {
  namespace unit_test {
    class test_suite;
  }
}



namespace com {



/*!
  \class PathNameTest
  \brief This class implements the unit tests for the PathName class.
*/
class PathNameTest
{

private:
  //! the platform dependant directory delimeter
  std::string d_slash;

public:

  //! Constructor.
                   PathNameTest        ();

  void             setUp               ();

  void             tearDown            ();

  void             testCtor            ();

  void             testSplitFor        ();

  void             testToString        ();

  void             testIsEmpty         ();

  void             testIsRelative      ();

  void             testMakeAbsolute    ();

  void             testUp              ();

  void             testMakeNative      ();

  void             testAdd             ();

  void             testExtension       ();

  void             testRemoveExtension ();

  void             testCompare         ();

  void             testEquals          ();

  void             testClear           ();

  void             testUnc             ();

  static boost::unit_test::test_suite* suite();
};

} // namespace com

#endif
