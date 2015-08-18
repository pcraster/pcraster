#ifndef INCLUDED_DAL_PCRBLOCKDRIVERTEST
#define INCLUDED_DAL_PCRBLOCKDRIVERTEST



// Library headers.

// PCRaster library headers.

// Module headers.



namespace boost {
  namespace unit_test {
    class test_suite;
  }
}

namespace dal {
  // PCRBlockDriver declarations.
  class Block;
}



namespace dal {



//! This class implements the unit tests for the PCRBlockDriver class.
class PCRBlockDriverTest
{

private:

  Block*           d_blockDiscretisation;

public:

                   PCRBlockDriverTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             testOpen            ();

  void             testRead            ();

  void             testWrite           ();

  static boost::unit_test::test_suite* suite();

};

} // namespace dal

#endif
