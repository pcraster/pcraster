#ifndef INCLUDED_PCRXML_ELEMENTTEST
#define INCLUDED_PCRXML_ELEMENTTEST



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif



namespace boost {
  namespace unit_test {
    class test_suite;
  }
}



namespace pcrxml {



//! This class implements the unit tests for the Element class.
class ElementTest
{

public:

                   ElementTest         ();

  void             setUp               ();

  void             tearDown            ();

  void             testToDomDocument   ();
  void             testWriteToFile     ();
  void             testRequired        ();

  static boost::unit_test::test_suite*    suite               ();

};

} // namespace pcrxml

#endif
