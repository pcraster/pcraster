#ifndef INCLUDED_PCRXML_INTERSPACEDSENTENCETEST
#define INCLUDED_PCRXML_INTERSPACEDSENTENCETEST



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



//! This class implements the unit tests for the InterSpacedSentence class.
class InterSpacedSentenceTest
{

public:

                   InterSpacedSentenceTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             testInterSpacedSentence           ();

  static boost::unit_test::test_suite*    suite               ();

};

} // namespace pcrxml

#endif
