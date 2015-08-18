#ifndef INCLUDED_DAL_TEXTMATRIXDRIVERTEST
#define INCLUDED_DAL_TEXTMATRIXDRIVERTEST



// Library headers.

// PCRaster library headers.

// Module headers.



namespace boost {
  namespace unit_test {
    class test_suite;
  }
}

namespace dal {
  // TextMatrixDriver declarations.
}



namespace dal {



//! This class implements the unit tests for the TextMatrixDriver class.
class TextMatrixDriverTest
{

private:

public:

                   TextMatrixDriverTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             testDescription     ();

  void             testUnexisting      ();

  void             testEmpty           ();

  void             testInvalidGrammar  ();

  void             testMatrix1         ();

  void             testMatrix2         ();

  void             testMatrix3         ();

  void             testMatrix4         ();

  static boost::unit_test::test_suite* suite();

};

} // namespace dal

#endif
