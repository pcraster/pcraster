#ifndef INCLUDED_DAL_STEPMAPPERTEST
#define INCLUDED_DAL_STEPMAPPERTEST



// Library headers.

// PCRaster library headers.

// Module headers.



namespace boost {
  namespace unit_test {
    class test_suite;
  }
}

namespace dal {
  // StepMapper declarations.
}



namespace dal {



//! This class implements the unit tests for the StepMapper class.
class StepMapperTest
{

private:

public:

                   StepMapperTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             test                ();

  static boost::unit_test::test_suite* suite();

};

} // namespace dal

#endif
