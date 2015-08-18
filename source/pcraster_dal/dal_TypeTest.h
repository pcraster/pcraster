#ifndef INCLUDED_DAL_TYPETEST
#define INCLUDED_DAL_TYPETEST



// Library headers.

// PCRaster library headers.

// Module headers.



namespace boost {
  namespace unit_test {
    class test_suite;
  }
}

namespace dal {
  // Type declarations.
}



namespace dal {



//! This class implements the unit tests for the Type class.
class TypeTest
{

private:

public:

                   TypeTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             testCanParseUint1   ();

  void             testCanParseUint2   ();

  void             testCanParseUint4   ();

  void             testCanParseInt1    ();

  void             testCanParseInt2    ();

  void             testCanParseInt4    ();

  void             testCanParseReal4   ();

  void             testCanParseReal8   ();

  void             testTypeTraits      ();

  void             testBasicType       ();

  void             testId              ();

  static boost::unit_test::test_suite* suite();

};

} // namespace dal

#endif
