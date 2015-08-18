#ifndef INCLUDED_COM_BITVECTORTEST
#define INCLUDED_COM_BITVECTORTEST



// Library headers.

// PCRaster library headers.

// Module headers.



namespace boost {
  namespace unit_test {
    class test_suite;
  }
}

namespace com {
  // BitVector declarations.
}



namespace com {



//! This class implements the unit tests for the BitVector class.
class BitVectorTest
{

public:

                   BitVectorTest       ();

  void             setUp               ();

  void             tearDown            ();

  void             test                ();

  static boost::unit_test::test_suite* suite();

};

} // namespace com

#endif
