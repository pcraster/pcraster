#ifndef INCLUDED_COM_RANGEMAPTEST
#define INCLUDED_COM_RANGEMAPTEST



// Library headers.

// PCRaster library headers.

// Module headers.



namespace boost {
  namespace unit_test {
    class test_suite;
  }
}

namespace com {
  // RangeMap declarations.
}



namespace com {



//! This class implements the unit tests for the RangeMap class.
class RangeMapTest
{

public:

                   RangeMapTest        ();

  void             setUp               ();

  void             tearDown            ();

  void             test                ();

  static boost::unit_test::test_suite* suite();

};

} // namespace com

#endif
