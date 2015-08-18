#ifndef INCLUDED_DAL_MAPPERUTILSTEST
#define INCLUDED_DAL_MAPPERUTILSTEST



// Library headers.

// PCRaster library headers.

// Module headers.



namespace boost {
  namespace unit_test {
    class test_suite;
  }
}

namespace dal {
  // MapperUtils declarations.
}



namespace dal {



//! This class implements the unit tests for the MapperUtils class.
class MapperUtilsTest
{

private:

public:

                   MapperUtilsTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             testStepMap         ();

  void             testTimeStepMap     ();

  static boost::unit_test::test_suite* suite();

};

} // namespace dal

#endif
