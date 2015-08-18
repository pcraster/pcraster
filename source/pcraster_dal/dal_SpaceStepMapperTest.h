#ifndef INCLUDED_DAL_SPACESTEPMAPPERTEST
#define INCLUDED_DAL_SPACESTEPMAPPERTEST



// External headers.

// Project headers.

// Module headers.



namespace boost {
  namespace unit_test {
    class test_suite;
  }
}

namespace dal {
  // SpaceStepMapperTest declarations.
}



namespace dal {

//! This class implements the unit tests for the SpaceStepMapper class.
class SpaceStepMapperTest
{

private:

public:

                   SpaceStepMapperTest           ();

  void             test                ();

  static boost::unit_test::test_suite* suite();

};

} // namespace dal

#endif
