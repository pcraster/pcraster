#ifndef INCLUDED_DAL_STACKINFOTEST
#define INCLUDED_DAL_STACKINFOTEST



// Library headers.

// PCRaster library headers.

// Module headers.



namespace boost {
  namespace unit_test {
    class test_suite;
  }
}

namespace dal {
  // StackInfo declarations.
}



namespace dal {



//! This class implements the unit tests for the StackInfo class.
/*!
  \todo      Add more tests with directories.
*/
class StackInfoTest
{

private:

public:

                   StackInfoTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             testConstructor     ();

  void             testBadFormats      ();

  void             testToString        ();

  void             testCopy            ();

  static boost::unit_test::test_suite* suite();

};

} // namespace dal

#endif
