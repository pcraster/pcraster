#ifndef INCLUDED_DAL_GEOEASTABLEDRIVERTEST
#define INCLUDED_DAL_GEOEASTABLEDRIVERTEST



// Library headers.

// PCRaster library headers.

// Module headers.



namespace boost {
  namespace unit_test {
    class test_suite;
  }
}

namespace dal {
  // GeoEASTableDriver declarations.
}



namespace dal {



//! This class implements the unit tests for the GeoEASTableDriver class.
class GeoEASTableDriverTest
{

private:

public:

                   GeoEASTableDriverTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             testDescription     ();

  void             testUnexisting      ();

  void             testEmpty           ();

  void             testInvalidGrammar  ();

  void             testTable1          ();

  void             testTable2          ();

  static boost::unit_test::test_suite* suite();

};

} // namespace dal

#endif
