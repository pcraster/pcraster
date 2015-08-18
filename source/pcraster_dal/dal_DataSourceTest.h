#ifndef INCLUDED_DAL_DATASOURCETEST
#define INCLUDED_DAL_DATASOURCETEST



// Library headers.

// PCRaster library headers.

// Module headers.



namespace boost {
  namespace unit_test {
    class test_suite;
  }
}

namespace dal {
  // DataSource declarations.
}



namespace dal {



//! This class implements the unit tests for the DataSource class.
class DataSourceTest
{

private:

public:

                   DataSourceTest      ();

  void             setUp               ();

  void             tearDown            ();

  void             testUnexisting      ();

  void             testSoil            ();

  void             test                ();

  void             testUniqueValues    ();

  void             testRaster          ();

  void             testDataset1        ();

  void             testDataset1Quantiles();

  void             testUncertainTemporalFeatureLayer();

  static boost::unit_test::test_suite* suite();

};

} // namespace dal

#endif
