#ifndef INCLUDED_DAL_MEMORYTABLEDRIVERTEST
#define INCLUDED_DAL_MEMORYTABLEDRIVERTEST



// Library headers.

// PCRaster library headers.

// Module headers.



namespace boost {
  namespace unit_test {
    class test_suite;
  }
}

namespace dal {
  // MemoryTableDriver declarations.
  class Table;
}



namespace dal {



//! This class implements the unit tests for the MemoryTableDriver class.
class MemoryTableDriverTest
{

private:

  Table*           d_table1;

  Table*           d_table2;

  Table*           d_table3;

public:

                   MemoryTableDriverTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             test                ();

  static boost::unit_test::test_suite* suite();

};

} // namespace dal

#endif
