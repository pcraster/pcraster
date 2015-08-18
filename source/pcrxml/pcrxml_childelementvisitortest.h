#ifndef INCLUDED_PCRXML_CHILDELEMENTVISITORTEST
#define INCLUDED_PCRXML_CHILDELEMENTVISITORTEST



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



//! This class implements the unit tests for the ChildElementVisitor class.
class ChildElementVisitorTest
{

public:

                   ChildElementVisitorTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             testCheckRequiredChild();
  void             testElementOnlyElement();

  static boost::unit_test::test_suite*    suite               ();

};

} // namespace pcrxml

#endif
