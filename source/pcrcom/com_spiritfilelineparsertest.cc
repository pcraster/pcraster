#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_COM_SPIRITFILELINEPARSERTEST
#include "com_spiritfilelineparsertest.h"
#define INCLUDED_COM_SPIRITFILELINEPARSERTEST
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
#ifndef INCLUDED_COM_SPIRITFILELINEPARSER
#include "com_spiritfilelineparser.h"
#define INCLUDED_COM_SPIRITFILELINEPARSER
#endif



/*!
  \file
  This file contains the implementation of the SpiritFileLineParserTest class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC SPIRITFILELINEPARSER MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*com::SpiritFileLineParserTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<SpiritFileLineParserTest> instance(new SpiritFileLineParserTest());

  suite->add(BOOST_CLASS_TEST_CASE(&SpiritFileLineParserTest::testOne, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&SpiritFileLineParserTest::testTwo, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&SpiritFileLineParserTest::testEmpty, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF SPIRITFILELINEPARSER MEMBERS
//------------------------------------------------------------------------------

//! ctor
com::SpiritFileLineParserTest::SpiritFileLineParserTest()
{
}

//! setUp
void com::SpiritFileLineParserTest::setUp()
{
}

//! tearDown
void com::SpiritFileLineParserTest::tearDown()
{
}

void com::SpiritFileLineParserTest::testOne()
{
  com::PathName pn("testparser.txt");
  const char *d[2] = {
   "1 2 3 4\n \n  5 A6\n", // \n at end
   "1 2 3 4\n \n  5 A6"   // no \n at end
  };

  for(size_t i=0;i < 2; ++i) {

    write(d[i],pn);
    SpiritFileLineParser sfp(pn);

    using namespace boost::spirit;
    std::vector<int> parsed;
    sfp.pi= parse(sfp.begin(),sfp.end(),
           +(int_p[append(parsed)]), space_p);
    BOOST_CHECK(parsed.size()==4);
    BOOST_CHECK(parsed[3]==4);
    BOOST_CHECK(sfp.fullMatch());

    sfp.advance();
#ifndef BORLANDC
    bool catched=false;
    try {
     sfp.pi= parse(sfp.begin(),sfp.end(),
           int_p[append(parsed)] >> int_p[append(parsed)], space_p);
     sfp.errorAtStop();
     } catch (const com::FilePositionError& cpe) {
         ;
      BOOST_CHECK(!sfp.fullMatch());
      BOOST_CHECK(cpe.lineNr()==3);
      BOOST_CHECK(cpe.columnNr()==5);
      BOOST_CHECK(cpe.messages().find("A6")!=std::string::npos);
      catched=true;

     }
    BOOST_CHECK(catched);
#endif
  }
#ifdef BORLANDC
  bool compileErrorIfdeffed(false);
  BOOST_CHECK(compileErrorIfdeffed);
#endif

}

void com::SpiritFileLineParserTest::testTwo()
{
  com::PathName pn("testparser.txt");
  const char *d[2] = {
   "1 2 3 4\n \n  5 6\n", // \n at end
   "1 2 3 4\n \n  5 6"   // no \n at end
  };

  for(size_t i=0;i < 2; ++i) {

    write(d[i],pn);
    SpiritFileLineParser sfp(pn);

    using namespace boost::spirit;
    std::vector<int> parsed;
    sfp.pi= parse(sfp.begin(),sfp.end(),
           +(int_p[append(parsed)]), space_p);
    BOOST_CHECK(parsed.size()==4);
    BOOST_CHECK(parsed[3]==4);
    BOOST_CHECK(sfp.fullMatch());

    sfp.advance();

    sfp.pi= parse(sfp.begin(),sfp.end(),
           int_p[append(parsed)] >> int_p[append(parsed)], space_p);
    BOOST_CHECK(sfp.fullMatch());
    BOOST_CHECK(parsed.size()==6);
    BOOST_CHECK(parsed[5]==6);

    sfp.advance();
    BOOST_CHECK(sfp.eof());

    // NO MORE INPUT
    sfp.pi= parse(sfp.begin(),sfp.end(),
           *(int_p[append(parsed)]), space_p);
    BOOST_CHECK(sfp.fullMatch());
    BOOST_CHECK(parsed.size()==6);
    BOOST_CHECK(parsed[5]==6);
  }
}

void com::SpiritFileLineParserTest::testEmpty()
{
  com::PathName pn("testparser.txt");
  const char *d[4] = {
   "\n", // \n at end
   "",   // empty
   "\n  \n \t  ", // only space
   "\n  \n \t  \n", // only space
  };

  for(size_t i=0;i < 4; ++i) {

    write(d[i],pn);
    SpiritFileLineParser sfp(pn);
    BOOST_CHECK(sfp.eof());

    using namespace boost::spirit;
    std::vector<int> parsed;
    sfp.pi= parse(sfp.begin(),sfp.end(),
           +(int_p[append(parsed)]), space_p);
    BOOST_CHECK(parsed.empty());
    BOOST_CHECK(sfp.fullMatch());
  }
}
