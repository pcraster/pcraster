#ifndef INCLUDED_DAL_CSFRASTERDRIVERTEST
#define INCLUDED_DAL_CSFRASTERDRIVERTEST



// Library headers.

// PCRaster library headers.

// Module headers.



namespace boost {
  namespace unit_test {
    class test_suite;
  }
}

namespace dal {
  // CSFRasterDriver declarations.
}



namespace dal {



//! This class implements the unit tests for the CSFRasterDriver class.
class CSFRasterDriverTest
{

private:

public:

                   CSFRasterDriverTest ();

  void             setUp               ();

  void             tearDown            ();

  void             testDescription     ();

  void             testUnexisting      ();

  void             testEmpty           ();

  void             testDtmSmall        ();

  void             testAccuLddIMap     ();

  void             testNames           ();

  void             testWrite           ();

  void             testProperties      ();

  void             testQuery           ();

  static boost::unit_test::test_suite* suite();

};

} // namespace dal

#endif
