#ifndef INCLUDED_COM_DIRECTORYTEST
#define INCLUDED_COM_DIRECTORYTEST

#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_COM_DIRECTORY
#include "com_directory.h"
#define INCLUDED_COM_DIRECTORY
#endif


namespace boost {
  namespace unit_test {
    class test_suite;
  }
}



namespace com {


//       1         2         3         4         5         6         7         8

//! This class implements the unit tests for the Directory class.
class DirectoryTest
{

private:

public:

                   DirectoryTest       ();

  void             setUp               ();

  void             tearDown            ();

  void             testCreateDirectory ();

  void             testDirectoryIterator();

  void             testEraseDirectory  ();

  static boost::unit_test::test_suite* suite();

};

} // namespace com

#endif
