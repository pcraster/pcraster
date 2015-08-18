#include "stddefx.h"

#include "pcrxml_document.h"

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

#ifndef INCLUDED_PCRXML_WORDTEST
#include "pcrxml_wordtest.h"
#define INCLUDED_PCRXML_WORDTEST
#endif

#ifndef INCLUDED_PCRXML_WORD
#include "pcrxml_word.h"
#define INCLUDED_PCRXML_WORD
#endif

#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif


/*!
  \file
  This file contains the implementation of the WordTest class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC CLASS MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*pcrxml::WordTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<WordTest> instance(new WordTest());

  suite->add(BOOST_CLASS_TEST_CASE(&WordTest::testWord, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF CLASS MEMBERS
//------------------------------------------------------------------------------

//! ctor
pcrxml::WordTest::WordTest()
{
}

//! construct from DOM Tree
void pcrxml::WordTest::testWord()
{
}
