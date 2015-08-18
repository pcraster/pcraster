#ifndef INCLUDED_DAL_MEMORYDATAPOOLTEST
#define INCLUDED_DAL_MEMORYDATAPOOLTEST



// Library headers.

// PCRaster library headers.

// Module headers.



namespace boost {
  namespace unit_test {
    class test_suite;
  }
}

namespace dal {
  // MemoryDataPool declarations.
}



namespace dal {



//! This class implements the unit tests for the MemoryDataPool class.
class MemoryDataPoolTest
{

private:

public:

                   MemoryDataPoolTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             testFillingOfDataSpace();

  static boost::unit_test::test_suite* suite();

};

} // namespace dal

#endif
