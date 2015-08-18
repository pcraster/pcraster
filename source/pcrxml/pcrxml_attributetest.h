#ifndef INCLUDED_PCRXML_ATTRIBUTETEST
#define INCLUDED_PCRXML_ATTRIBUTETEST



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



//! This class implements the unit tests for the Attribute class.
class AttributeTest
{

public:

                   AttributeTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             testCheckRequiredAttribute();
  void             testForwardSlash();

  static boost::unit_test::test_suite*    suite               ();

};

} // namespace pcrxml

#endif
