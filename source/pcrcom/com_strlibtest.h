#ifndef INCLUDED_COM_STRLIBTEST
#define INCLUDED_COM_STRLIBTEST

#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif


namespace boost {
  namespace unit_test {
    class test_suite;
  }
}

namespace com {



/*!
  \class StrLibTest
  \brief This class implements the unit tests for the StrLib class.
*/
//       1         2         3         4         5         6         7         8
class StrLibTest
{

private:

public:

                   StrLibTest          ();

  void             setUp               ();

  void             tearDown            ();

  void             testFromString      ();
  void             testToString        ();
  void             testIsNumber        ();
  void             testCompareNoCase   ();

  void             testRemoveFrontEndSpace();
  void             testRemoveAllSpace  ();

  void             testRemoveFrontEndChar();

  void             testRemoveFrontEndString();

  void             testSplitAndJoin    ();
  void             testSplitAtChar     ();

  void             testDuplicateAsCStr ();

  void             testEqualNoSpace    ();

  void             testStringLess      ();
  void             testReplaceChars    ();
  void             testReplaceStrByStr ();

  void             testFormat          ();

  static boost::unit_test::test_suite* suite();

};

} // namespace com

#endif
