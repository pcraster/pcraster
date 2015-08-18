#ifndef INCLUDED_DAL_TABLEDRIVERTEST
#define INCLUDED_DAL_TABLEDRIVERTEST



// Library headers.

// PCRaster library headers.

// Module headers.



namespace boost {
  namespace unit_test {
    class test_suite;
  }
}

namespace dal {
  // TableDriver declarations.
}



namespace dal {



//! This class implements the unit tests for the TableDriver class.
class TableDriverTest
{

private:

public:

                   TableDriverTest           ();

  void             setUp               ();

  void             tearDown            ();

  static boost::unit_test::test_suite* suite();

};

} // namespace dal

#endif
