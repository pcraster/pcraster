#ifndef INCLUDED_PCRXML_WORDTEST
#define INCLUDED_PCRXML_WORDTEST



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



//! This class implements the unit tests for the Word class.
class WordTest
{

public:

                   WordTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             testWord            ();

  static boost::unit_test::test_suite*   suite               ();

};

} // namespace pcrxml

#endif
