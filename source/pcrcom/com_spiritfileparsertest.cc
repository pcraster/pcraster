#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_COM_SPIRITFILEPARSERTEST
#include "com_spiritfileparsertest.h"
#define INCLUDED_COM_SPIRITFILEPARSERTEST
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
#ifndef INCLUDED_COM_PATHNAME
#include "com_pathname.h"
#define INCLUDED_COM_PATHNAME
#endif
#ifndef INCLUDED_COM_FILE
#include "com_file.h"
#define INCLUDED_COM_FILE
#endif
#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif

// Module headers.
#ifndef INCLUDED_COM_SPIRITFILEPARSER
#include "com_spiritfileparser.h"
#define INCLUDED_COM_SPIRITFILEPARSER
#endif



/*!
  \file
  This file contains the implementation of the SpiritFileParserTest class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC SPIRITFILEPARSER MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*com::SpiritFileParserTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<SpiritFileParserTest> instance(new SpiritFileParserTest());

  suite->add(BOOST_CLASS_TEST_CASE(&SpiritFileParserTest::testCurrent, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&SpiritFileParserTest::fileMapTooLarge, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF SPIRITFILEPARSER MEMBERS
//------------------------------------------------------------------------------

//! ctor
com::SpiritFileParserTest::SpiritFileParserTest()
{
}

//! setUp
void com::SpiritFileParserTest::setUp()
{
}

//! tearDown
void com::SpiritFileParserTest::tearDown()
{
}


//! test the idea of saving the current position to resume later
void com::SpiritFileParserTest::testCurrent()
{
  com::PathName pn("testparser.txt");
  com::write("1 2 3 4\n \n 5 A6",pn);
  std::vector<int> parsed;

  SpiritFileParser sfp(pn);

  BOOST_CHECK(sfp.begin()==sfp.current());
  using namespace boost::spirit::classic;
  sfp.pi= parse(sfp.begin(),sfp.end(),
         int_p[append(parsed)] >> int_p[append(parsed)], space_p);
  BOOST_CHECK(!sfp.pi.full);
  BOOST_CHECK(parsed.size()==2);
  sfp.advance();

  sfp.pi= parse(sfp.current(),sfp.end(),
         int_p[append(parsed)] >> int_p[append(parsed)], space_p);
  sfp.advance();
  BOOST_CHECK(parsed.size()==4);
  BOOST_CHECK(parsed[3]==4);

  bool catched(false);
  try {
   sfp.pi= parse(sfp.current(),sfp.end(),
         int_p[append(parsed)] >> int_p[append(parsed)], space_p);
   sfp.errorAtStop();
  } catch (const com::FilePositionError& cpe) {
    BOOST_CHECK(cpe.lineNr()==3);
    BOOST_CHECK(cpe.columnNr()==4);
    BOOST_WARN_MESSAGE(cpe.messages().find("A6") != std::string::npos,
     std::string("EXPECT A6 to be part of message ")+cpe.messages());
    catched=true;
  }
  BOOST_CHECK(catched);
}

void com::SpiritFileParserTest::fileMapTooLarge()
{
#ifdef WIN32
  com::PathName big("E:\\gam_allXL.xyz");
#else
  com::PathName big("/home/cees/tmp/gam_allXL.xyz");
#endif
  if (com::exists(big)) {
   bool catched(false);
   try {
    SpiritFileParser n(big);
   } catch(const com::OpenFileError& c) {
     BOOST_CHECK(c.messages().find("large") != std::string::npos);
     catched=true;
   }
   BOOST_CHECK(catched);
  }
}
