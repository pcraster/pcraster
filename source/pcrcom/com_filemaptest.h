#ifndef INCLUDED_COM_FILEMAPTEST
#define INCLUDED_COM_FILEMAPTEST



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
  // FileMap declarations.
}



namespace com {



//! This class implements the unit tests for the FileMap class.
class FileMapTest
{

public:

                   FileMapTest         ();

  void             setUp               ();

  void             tearDown            ();

  void             testEmptyFile       ();

  void             testIterators       ();

  void             fileMapToLarge      ();

  static boost::unit_test::test_suite* suite();

};

} // namespace com

#endif
