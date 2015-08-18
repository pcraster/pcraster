#ifndef INCLUDED_DAL_CSFMAPTEST
#define INCLUDED_DAL_CSFMAPTEST



// Library headers.

// PCRaster library headers.

// Module headers.



namespace boost {
  namespace unit_test {
    class test_suite;
  }
}

namespace dal {
  // CSFMap declarations.
}



namespace dal {



//! This class implements the unit tests for the CSFMap class.
class CSFMapTest
{

private:

public:

                   CSFMapTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             testRead            ();

  void             testCreate          ();

  void             testError           ();

  void             testLegend          ();

  static boost::unit_test::test_suite* suite();

};

} // namespace dal

#endif
