#ifndef INCLUDED_DAL_BLOCKTEST
#define INCLUDED_DAL_BLOCKTEST



// Library headers.

// PCRaster library headers.

// Module headers.



namespace boost {
  namespace unit_test {
    class test_suite;
  }
}

namespace dal {
  // Block declarations.
}



namespace dal {



//! This class implements the unit tests for the Block class.
class BlockTest
{

private:

public:

                   BlockTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             test                ();

  static boost::unit_test::test_suite* suite();

};

} // namespace dal

#endif
