#ifndef INCLUDED_DAL_TEXTTABLEDRIVERTEST
#define INCLUDED_DAL_TEXTTABLEDRIVERTEST



// Library headers.

// PCRaster library headers.

// Module headers.



namespace boost {
  namespace unit_test {
    class test_suite;
  }
}

namespace dal {
  // TextTableDriver declarations.
}



namespace dal {



//! This class implements the unit tests for the TextTableDriver class.
class TextTableDriverTest
{

private:

public:

                   TextTableDriverTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             test                ();

  void             testDescription     ();

  void             testUnexisting      ();

  void             testEmpty           ();

  void             testInvalidGrammar  ();

  void             testTable1          ();

  void             testTable2          ();

  void             testTable5          ();

  void             testTable6          ();

  void             testTable2eas       ();

  void             testDOSFormattedTable();

  void             testColumnWithEmptyValues();

  void             testColumnWithQuiteSomeZeros();

  static boost::unit_test::test_suite* suite();

};

} // namespace dal

#endif
