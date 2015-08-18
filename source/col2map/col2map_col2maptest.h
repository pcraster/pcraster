#ifndef INCLUDED_COL2MAP_COL2MAPTEST
#define INCLUDED_COL2MAP_COL2MAPTEST



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif



// Library headers.
#ifndef INCLUDED_BOOST_TEST_UNIT_TEST
#include <boost/test/unit_test.hpp>
#define INCLUDED_BOOST_TEST_UNIT_TEST
#endif

// PCRaster library headers.

// Module headers.



namespace boost {
  namespace unit_test {
    class test_suite;
  }
}


namespace col2map {
  // Col2Map declarations.
}





namespace col2map {

//! This class implements the unit tests for the Col2Map class.
class Col2MapTest
{

private:

public:

                   Col2MapTest         ();

  void             setUp               ();

  void             tearDown            ();

  void             testNaN             ();

  static boost::unit_test::test_suite* suite();

};

} // namespace col2map

#endif
