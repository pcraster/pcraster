#ifndef INCLUDED_GEO_CSFSTACKNAMETEST
#define INCLUDED_GEO_CSFSTACKNAMETEST



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif



namespace boost {
  namespace unit_test {
    class test_suite;
  }
}



namespace geo {



/*!
  \class CSFStackNameTest
  \brief This class implements the unit tests for the CSFStackName class.
*/
//       1         2         3         4         5         6         7         8
class CSFStackNameTest
{
private:

public:

  static boost::unit_test::test_suite* suite();

                   CSFStackNameTest    ();

  void             setUp               ();

  void             tearDown            ();

  void             testConstructor     ();

  void             testBadFormats      ();

  void             testBaseName        ();

  void             testFileName        ();

  void             testNrLayers        ();

  void             testAsAguilaArgument();

};

} // namespace geo

#endif
