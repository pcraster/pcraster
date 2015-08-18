#ifndef INCLUDED_DAL_TABLETEST
#define INCLUDED_DAL_TABLETEST



// Library headers.

// PCRaster library headers.

// Module headers.



namespace boost {
  namespace unit_test {
    class test_suite;
  }
}

namespace dal {
  // Table declarations.
  class Table;
}



namespace dal {



//! This class implements the unit tests for the Table class.
class TableTest
{

private:

  Table*           d_empty;

  Table*           d_table1;

  void             testEmptyTable      (Table const& table);

  void             testTable1          (Table const& table);

public:

                   TableTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             testConstructor     ();

  void             testCopy            ();

  void             testAssign          ();

  static boost::unit_test::test_suite* suite();

};

} // namespace dal

#endif
