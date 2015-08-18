#ifndef INCLUDED_DAL_TIMESTEPMAPPERTEST
#define INCLUDED_DAL_TIMESTEPMAPPERTEST



// Library headers.

// PCRaster library headers.

// Module headers.



namespace boost {
  namespace unit_test {
    class test_suite;
  }
}

namespace dal {
  // TimeStepMapper declarations.
}



namespace dal {



//! This class implements the unit tests for the TimeStepMapper class.
class TimeStepMapperTest
{

private:

public:

                   TimeStepMapperTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             test                ();

  static boost::unit_test::test_suite* suite();

};

} // namespace dal

#endif
