#ifndef INCLUDED_DAL_DALTEST
#define INCLUDED_DAL_DALTEST



// Library headers.
#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif

// PCRaster library headers.

// Module headers.



namespace boost {
  namespace unit_test {
    class test_suite;
  }
}

namespace dal {
  // Dal declarations.
}



namespace dal {



//! This class implements the unit tests for the Dal class.
class DalTest
{

private:

  void             testNotExisting     (std::string const& name);

public:

                   DalTest             ();

  void             setUp               ();

  void             tearDown            ();

  void             testNotExisting     ();

  void             testTable           ();

  void             testDatasetDriverTupleManagement();

  static boost::unit_test::test_suite* suite();

};

} // namespace dal

#endif
