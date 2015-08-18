#ifndef INCLUDED_DAL_MEMORYRASTERDATATEST
#define INCLUDED_DAL_MEMORYRASTERDATATEST



// Library headers.

// PCRaster library headers.

// Module headers.



namespace boost {
  namespace unit_test {
    class test_suite;
  }
}

namespace dal {
  // MemoryRasterData declarations.
}



namespace dal {



//! This class implements the unit tests for the MemoryRasterData class.
class MemoryRasterDataTest
{

private:

public:

                   MemoryRasterDataTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             test                ();

  static boost::unit_test::test_suite* suite();

};

} // namespace dal

#endif
