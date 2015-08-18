#ifndef INCLUDED_PALETTETEST
#define INCLUDED_PALETTETEST



// External headers.

// Project headers.

// Module headers.



namespace boost {
  namespace unit_test {
    class test_suite;
  }
}

namespace ag {
  // PaletteTest declarations.
}



namespace ag {

//! This class implements the unit tests for the Palette class.
class PaletteTest
{

private:

public:

                   PaletteTest         ();

  void             test                ();

  void             testCopy            ();

  static boost::unit_test::test_suite* suite();

};

} // namespace ag

#endif
