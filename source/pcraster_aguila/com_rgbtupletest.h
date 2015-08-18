#ifndef INCLUDED_COM_RGBTUPLETEST
#define INCLUDED_COM_RGBTUPLETEST



// Library headers.

// PCRaster library headers.

// Module headers.



namespace boost {
  namespace unit_test {
    class test_suite;
  }
}

namespace com {
  // RgbTuple declarations.
}



namespace com {



//! This class implements the unit tests for the RgbTuple class.
class RgbTupleTest
{

public:

                   RgbTupleTest        ();

  void             setUp               ();

  void             tearDown            ();

  void             testEq              ();

  static boost::unit_test::test_suite* suite();

};

} // namespace com

#endif
