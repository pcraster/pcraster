#ifndef INCLUDED_DAL_USECASESTEST
#define INCLUDED_DAL_USECASESTEST



// Library headers.

// PCRaster library headers.

// Module headers.



namespace boost {
  namespace unit_test {
    class test_suite;
  }
}

namespace dal {
  // UseCases declarations.
}



namespace dal {



//! This class implements the unit tests for the UseCases class.
class UseCasesTest
{

private:

public:

                   UseCasesTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             test1               ();

  void             testBilFormat       ();

  static boost::unit_test::test_suite* suite();

};

} // namespace dal

#endif
