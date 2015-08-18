#ifndef INCLUDED_DAL_DEFTEST
#define INCLUDED_DAL_DEFTEST



// Library headers.

// PCRaster library headers.

// Module headers.



namespace boost {
  namespace unit_test {
    class test_suite;
  }
}

namespace dal {
  // Def declarations.
}



namespace dal {



//! This class implements the unit tests for the Def class.
class DefTest
{

private:

public:

                   DefTest             ();

  void             setUp               ();

  void             tearDown            ();

  void             testTypeSizes       ();

  static boost::unit_test::test_suite* suite();

};

} // namespace dal

#endif
