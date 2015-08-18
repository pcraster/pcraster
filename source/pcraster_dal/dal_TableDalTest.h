#ifndef INCLUDED_DAL_TABLEDALTEST
#define INCLUDED_DAL_TABLEDALTEST



// Library headers.

// PCRaster library headers.

// Module headers.



namespace boost {
  namespace unit_test {
    class test_suite;
  }
}

namespace dal {
  // TableDalTest declarations.
}



namespace dal {

//! This class implements the unit tests for the TableDal class.
class TableDalTest
{

private:

public:

                   TableDalTest        ();

  void             setUp               ();

  void             tearDown            ();

  void             test                ();

  static boost::unit_test::test_suite* suite();

};

} // namespace dal

#endif
