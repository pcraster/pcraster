#ifndef INCLUDED_DAL_MEMORYRASTERDRIVERTEST
#define INCLUDED_DAL_MEMORYRASTERDRIVERTEST



// Library headers.

// PCRaster library headers.

// Module headers.



namespace boost {
  namespace unit_test {
    class test_suite;
  }
}

namespace dal {
  // MemoryRasterDriver declarations.
}



namespace dal {



//! This class implements the unit tests for the MemoryRasterDriver class.
class MemoryRasterDriverTest
{

private:

public:

                   MemoryRasterDriverTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             testEmptyDataSpace  ();

  void             testSameName        ();

  void             test                ();

  static boost::unit_test::test_suite* suite();

};

} // namespace dal

#endif
