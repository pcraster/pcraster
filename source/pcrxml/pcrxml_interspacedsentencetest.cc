#include "stddefx.h"
#ifndef INCLUDED_PCRXML_INTERSPACEDSENTENCETEST
#include "pcrxml_interspacedsentencetest.h"
#define INCLUDED_PCRXML_INTERSPACEDSENTENCETEST
#endif


#ifndef INCLUDED_PCRXML_INTERSPACEDSENTENCE
#include "pcrxml_interspacedsentence.h"
#define INCLUDED_PCRXML_INTERSPACEDSENTENCE
#endif

#ifndef INCLUDED_PCRXML_DOCUMENT
#include "pcrxml_document.h"
#define INCLUDED_PCRXML_DOCUMENT
#endif

#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
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



/*!
  \file
  This file contains the implementation of the InterSpacedSentenceTest class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC CLASS MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*pcrxml::InterSpacedSentenceTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<InterSpacedSentenceTest> instance(new InterSpacedSentenceTest());

  suite->add(BOOST_CLASS_TEST_CASE(&InterSpacedSentenceTest::testInterSpacedSentence, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF CLASS MEMBERS
//------------------------------------------------------------------------------

//! ctor
pcrxml::InterSpacedSentenceTest::InterSpacedSentenceTest()
{
}

//! test
void pcrxml::InterSpacedSentenceTest::testInterSpacedSentence()
{
}
