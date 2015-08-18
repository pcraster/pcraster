#ifndef INCLUDED_DAL_TEXTFILEDRIVERTEST
#define INCLUDED_DAL_TEXTFILEDRIVERTEST



// Library headers.

// PCRaster library headers.

// Module headers.



namespace boost {
  namespace unit_test {
    class test_suite;
  }
}

namespace dal {
  // TextFileDriver declarations.
}



namespace dal {



//! This class implements the unit tests for the TextFileDriver class.
class TextFileDriverTest
{

private:

public:

                   TextFileDriverTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             testDetermineTypeId ();

  static boost::unit_test::test_suite* suite();

};

} // namespace dal

#endif
