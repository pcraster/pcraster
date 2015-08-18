#ifndef INCLUDED_DAL_CONSTANTTEST
#define INCLUDED_DAL_CONSTANTTEST



// External headers.

// Project headers.

// Module headers.



namespace boost {
  namespace unit_test {
    class test_suite;
  }
}

namespace dal {
  // ConstantTest declarations.
}



namespace dal {

//! This class implements the unit tests for the Constant class.
class ConstantTest
{

private:

public:

                   ConstantTest           ();

  void             test                ();

  static boost::unit_test::test_suite* suite();

};

} // namespace dal

#endif
