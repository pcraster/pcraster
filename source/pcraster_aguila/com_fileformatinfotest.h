#ifndef INCLUDED_COM_FILEFORMATINFOTEST
#define INCLUDED_COM_FILEFORMATINFOTEST



// Library headers.

// PCRaster library headers.

// Module headers.



namespace boost {
  namespace unit_test {
    class test_suite;
  }
}

namespace com {
  // FileFormatInfo declarations.
}



namespace com {



//! This class implements the unit tests for the FileFormatInfo class.
class FileFormatInfoTest
{

public:

                   FileFormatInfoTest  ();

  void             setUp               ();

  void             tearDown            ();

  void             testConstructor     ();

  static boost::unit_test::test_suite* suite();

};

} // namespace com

#endif
