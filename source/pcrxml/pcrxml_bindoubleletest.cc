#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_PCRXML_BINDOUBLELETEST
#include "pcrxml_bindoubleletest.h"
#define INCLUDED_PCRXML_BINDOUBLELETEST
#endif

// Library headers.
#ifndef INCLUDED_BOOST_SHARED_PTR
#include <boost/shared_ptr.hpp>
#define INCLUDED_BOOST_SHARED_PTR
#endif

#ifndef INCLUDED_BOOST_TEST_TEST_TOOLS
#include <boost/test/test_tools.hpp>
#define INCLUDED_BOOST_TEST_TEST_TOOLS
#endif

#ifndef INCLUDED_BOOST_TEST_UNIT_TEST_SUITE
#include <boost/test/unit_test_suite.hpp>
#define INCLUDED_BOOST_TEST_UNIT_TEST_SUITE
#endif

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_PCRXML_BINDOUBLELE
#include "pcrxml_bindoublele.h"
#define INCLUDED_PCRXML_BINDOUBLELE
#endif



/*!
  \file
  This file contains the implementation of the BinDoubleLETest class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC BINDOUBLELE MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*pcrxml::BinDoubleLETest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<BinDoubleLETest> instance(new BinDoubleLETest());

  suite->add(BOOST_CLASS_TEST_CASE(&BinDoubleLETest::testEncoding, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF BINDOUBLELE MEMBERS
//------------------------------------------------------------------------------

//! ctor
pcrxml::BinDoubleLETest::BinDoubleLETest()
{
}

void pcrxml::BinDoubleLETest::testEncoding()
{
  {
    BinDoubleLE v(0);
    BOOST_CHECK(v.attrValueStr() == "0000000000000000");
  }
  {
    BinDoubleLE v(1234.34);
    double cmpV = v();
    BOOST_CHECK(cmpV == BinDoubleLE::hexToDouble(v.attrValueStr()));
  }
  {
    BinDoubleLE v(0.001);
    double cmpV = v();
    BOOST_CHECK(cmpV == BinDoubleLE::hexToDouble(v.attrValueStr()));
    BOOST_CHECK(cmpV == 0.001);
  }
}
