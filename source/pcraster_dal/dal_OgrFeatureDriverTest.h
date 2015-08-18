#ifndef INCLUDED_DAL_OGRFEATUREDRIVERTEST
#define INCLUDED_DAL_OGRFEATUREDRIVERTEST

// Library headers.

// PCRaster library headers.

// Module headers.



namespace boost {
  namespace unit_test {
    class test_suite;
  }
}

namespace dal {
  // OgrFeatureDriverTest declarations.
}



namespace dal {

//! This class implements the unit tests for the OgrFeatureDriver class.
class OgrFeatureDriverTest
{

private:

public:

                   OgrFeatureDriverTest();

  void             testExists          ();

  void             testOpen            ();

  void             testDataSpace       ();

  void             testRead            ();

  void             testQuery           ();

  void             testAttributes      ();

  void             testSpace           ();

  void             testTime            ();

  void             testUncertainty     ();

  void             testUncertainTemporal();

  static boost::unit_test::test_suite* suite();

};

} // namespace dal

#endif
