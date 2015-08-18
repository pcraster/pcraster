#ifndef INCLUDED_PCRXML_DOCUMENTTEST
#define INCLUDED_PCRXML_DOCUMENTTEST



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



//! This class implements the unit tests for the Document class.
class DocumentTest
{

public:

                   DocumentTest          ();

  void             setUp               ();

  void             tearDown            ();

  void             testPcrDocument     ();

  void             testNotExistant     ();

  void             testCtorAndParser   ();

  void             testFirstMatchByTagName();

  void             testNameSpaceStuff();

  static boost::unit_test::test_suite*    suite               ();

};

} // namespace pcrxml

#endif
