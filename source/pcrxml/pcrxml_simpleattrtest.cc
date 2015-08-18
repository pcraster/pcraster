#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_PCRXML_SIMPLEATTRTEST
#include "pcrxml_simpleattrtest.h"
#define INCLUDED_PCRXML_SIMPLEATTRTEST
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
#ifndef INCLUDED_PCRGENXML_DIRECTORYSTACKINFO
#include "pcrgenxml_directorystackinfo.h"
#define INCLUDED_PCRGENXML_DIRECTORYSTACKINFO
#endif
#ifndef INCLUDED_PCRXML_DOCUMENT
#include "pcrxml_document.h"
#define INCLUDED_PCRXML_DOCUMENT
#endif



/*!
  \file
  This file contains the implementation of the SimpleAttrTest class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC SIMPLEATTR MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*pcrxml::SimpleAttrTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<SimpleAttrTest> instance(new SimpleAttrTest());

  suite->add(BOOST_CLASS_TEST_CASE(&SimpleAttrTest::testIt, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF SIMPLEATTR MEMBERS
//------------------------------------------------------------------------------

//! ctor
pcrxml::SimpleAttrTest::SimpleAttrTest()
{
}



//! test all of Simple Attr template
void pcrxml::SimpleAttrTest::testIt()
{
      Document doc("<DirectoryStackInfo "
                     "allMissingValue='false' "
                     "minimumValue='1.45' "
                     "maximumValue='2.67' "
                     "dataTypeDTD='Scalar' "
                     "stackEnd='3' />");
      DirectoryStackInfo d(doc.documentElement());
      BOOST_CHECK(!d.allMissingValue());
      BOOST_CHECK(d.minimumValue()==1.45);
      BOOST_CHECK(d.maximumValue()==2.67);
      BOOST_CHECK(d.dataTypeDTD()==DataTypeEnum::Scalar);
      BOOST_CHECK(d.stackEnd()==3);
}
