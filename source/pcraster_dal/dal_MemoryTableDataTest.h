#ifndef INCLUDED_DAL_MEMORYTABLEDATATEST
#define INCLUDED_DAL_MEMORYTABLEDATATEST



// Library headers.

// PCRaster library headers.

// Module headers.



namespace boost {
  namespace unit_test {
    class test_suite;
  }
}

namespace dal {
  // MemoryTableData declarations.
}



namespace dal {



//! This class implements the unit tests for the MemoryTableData class.
class MemoryTableDataTest
{

private:

public:

                   MemoryTableDataTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             testConstructor     ();

  static boost::unit_test::test_suite* suite();

};

} // namespace dal

#endif
