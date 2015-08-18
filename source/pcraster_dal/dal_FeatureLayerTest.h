#ifndef INCLUDED_DAL_FEATURELAYERTEST
#define INCLUDED_DAL_FEATURELAYERTEST



// Library headers.

// PCRaster library headers.

// Module headers.



namespace boost {
  namespace unit_test {
    class test_suite;
  }
}

namespace dal {
  // FeatureLayerTest declarations.
}



namespace dal {

//! This class implements the unit tests for the FeatureLayer class.
class FeatureLayerTest
{

private:

public:

                   FeatureLayerTest           ();

  void             test                ();

  static boost::unit_test::test_suite* suite();

};

} // namespace dal

#endif
