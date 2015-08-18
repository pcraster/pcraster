#ifndef INCLUDED_DAL_VTKBLOCKDRIVERTEST
#define INCLUDED_DAL_VTKBLOCKDRIVERTEST



// Library headers.

// PCRaster library headers.

// Module headers.



namespace boost {
  namespace unit_test {
    class test_suite;
  }
}

namespace dal {
  // VTKBlockDriverTest declarations.
}



namespace dal {

//! This class implements the unit tests for the VTKBlockDriver class.
class VTKBlockDriverTest
{

private:

public:

                   VTKBlockDriverTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             test                ();

  static boost::unit_test::test_suite* suite();

};

} // namespace dal

#endif
