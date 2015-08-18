#include "stddefx.h"
#include "pcrxml_enumnmtokentest.h"

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

#ifndef INCLUDED_PCRGENXML_TESTEMPTYELEMENTNOATTR
#include "pcrgenxml_testemptyelementnoattr.h"
#define INCLUDED_PCRGENXML_TESTEMPTYELEMENTNOATTR
#endif

#ifndef INCLUDED_PCRXML_DOCUMENT
#include "pcrxml_document.h"
#define INCLUDED_PCRXML_DOCUMENT
#endif

#ifndef INCLUDED_PCRXML_STRINGCONV
#include "pcrxml_stringconv.h"
#define INCLUDED_PCRXML_STRINGCONV
#endif

#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif

#ifndef INCLUDED_IOSTREAM
#include <iostream>
#define INCLUDED_IOSTREAM
#endif


/*!
  \file
  This file contains the implementation of the EnumNmTokenTest class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC CLASS MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*pcrxml::EnumNmTokenTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<EnumNmTokenTest> instance(new EnumNmTokenTest());

  suite->add(BOOST_CLASS_TEST_CASE(&EnumNmTokenTest::testEnum, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF CLASS MEMBERS
//------------------------------------------------------------------------------

//! ctor
pcrxml::EnumNmTokenTest::EnumNmTokenTest()
{
}

//! test EnumNmToken by an instance
void pcrxml::EnumNmTokenTest::testEnum()
{
}
