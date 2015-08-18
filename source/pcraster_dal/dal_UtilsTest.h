#ifndef INCLUDED_DAL_UTILSTEST
#define INCLUDED_DAL_UTILSTEST



// Library headers.

// PCRaster library headers.

// Module headers.



namespace boost {
  namespace unit_test {
    class test_suite;
  }
}

namespace dal {
  // Utils declarations.
}



namespace dal {



//! This class implements the unit tests for the Utils class.
class UtilsTest
{

private:

public:

                   UtilsTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             testDatasetType     ();

  void             testSplitNameAndSelection();

  void             testDataSpaceToString();

  void             testDataSpaceToFieldNames();

  void             testDataSpaceAddressToSqlQuery();

  static boost::unit_test::test_suite* suite();

};

} // namespace dal

#endif
