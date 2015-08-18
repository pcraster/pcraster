#ifndef INCLUDED_PCRXML_PCDATAELEMENTTEST
#define INCLUDED_PCRXML_PCDATAELEMENTTEST



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.

// Module headers.



namespace boost {
  namespace unit_test {
    class test_suite;
  }
}

namespace pcrxml {
  // PCDATAElement declarations.
}



namespace pcrxml {



//! This class implements the unit tests for the PCDATAElement class.
class PCDATAElementTest
{

private:

public:

                   PCDATAElementTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             test                ();

  static boost::unit_test::test_suite* suite();

};

} // namespace pcrxml

#endif
