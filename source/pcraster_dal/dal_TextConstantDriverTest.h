#ifndef INCLUDED_DAL_TEXTCONSTANTDRIVERTEST
#define INCLUDED_DAL_TEXTCONSTANTDRIVERTEST

// External headers.

// Project headers.

// Module headers.



namespace boost {
  namespace unit_test {
    class test_suite;
  }
}

namespace dal {
  // TextConstantDriverTest declarations.
}



namespace dal {

//! This class implements the unit tests for the TextConstantDriver class.
class TextConstantDriverTest
{

private:

public:

                   TextConstantDriverTest();

  void             testDescription     ();

  void             testUnexisting      ();

  static boost::unit_test::test_suite* suite();

};

} // namespace dal

#endif
