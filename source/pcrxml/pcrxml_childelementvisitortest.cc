#include "stddefx.h"

#ifndef INCLUDED_PCRXML_CHILDELEMENTVISITORTEST
#include "pcrxml_childelementvisitortest.h"
#define INCLUDED_PCRXML_CHILDELEMENTVISITORTEST
#endif


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

#ifndef INCLUDED_PCRXML_DOCUMENT
#include "pcrxml_document.h"
#define INCLUDED_PCRXML_DOCUMENT
#endif

#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif

/*!
  \file
  This file contains the implementation of the ChildElementVisitorTest class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC CLASS MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*pcrxml::ChildElementVisitorTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<ChildElementVisitorTest> instance(new ChildElementVisitorTest());

  suite->add(BOOST_CLASS_TEST_CASE(&ChildElementVisitorTest::testCheckRequiredChild, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&ChildElementVisitorTest::testElementOnlyElement, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF CLASS MEMBERS
//------------------------------------------------------------------------------

//! ctor
pcrxml::ChildElementVisitorTest::ChildElementVisitorTest()
{
}

//! test
void pcrxml::ChildElementVisitorTest::testCheckRequiredChild()
{
}

//! test by parsing an element that uses ChildElementVisitor
void pcrxml::ChildElementVisitorTest::testElementOnlyElement()
{
}
