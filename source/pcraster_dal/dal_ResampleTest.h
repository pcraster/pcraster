#ifndef INCLUDED_DAL_RESAMPLETEST
#define INCLUDED_DAL_RESAMPLETEST



// External headers.

// Project headers.

// Module headers.



namespace boost {
  namespace unit_test {
    class test_suite;
  }
}

namespace dal {
  // ResampleTest declarations.
}



namespace dal {

//! This class implements the unit tests for the Resample class.
class ResampleTest
{

private:

public:

                   ResampleTest        ();

  void             testOverlap         ();

  static boost::unit_test::test_suite* suite();

};

} // namespace dal

#endif
