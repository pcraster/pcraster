#ifndef INCLUDED_PCRXML_ENUMNMTOKENTEST
#define INCLUDED_PCRXML_ENUMNMTOKENTEST



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



//! This class implements the unit tests for the EnumNmToken class.
/*! EnumNmTokenTest is an overall test, parsing pieces of the 
    PCRaster dtd/schema
 */
class EnumNmTokenTest
{

public:

                   EnumNmTokenTest   ();

  void             setUp               ();

  void             tearDown            ();

  void             testEnum            ();

  static boost::unit_test::test_suite*    suite               ();

};

} // namespace pcrxml

#endif
