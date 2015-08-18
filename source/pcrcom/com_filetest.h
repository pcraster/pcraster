#ifndef INCLUDED_COM_FILETEST
#define INCLUDED_COM_FILETEST

#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_COM_FILE
#include "com_file.h"
#define INCLUDED_COM_FILE
#endif


namespace boost {
  namespace unit_test {
    class test_suite;
  }
}



namespace com {


//       1         2         3         4         5         6         7         8

//! This class implements the unit tests for the File
class FileTest
{

private:

public:

                   FileTest            ();

  void             setUp               ();

  void             tearDown            ();

  void             testMove            ();

  void             testRemoveFile      ();

  void             testOpenIfStream    ();

  void             testOpenOfStream    ();

  void             testFilesEqual      ();

  void             testCopy            ();

  void             testReadWriteSize   ();

  void             testSkipWhiteSpace  ();

  static boost::unit_test::test_suite* suite();

};

} // namespace com

#endif
