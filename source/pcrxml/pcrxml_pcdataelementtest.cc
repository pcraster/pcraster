#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_PCRXML_PCDATAELEMENTTEST
#include "pcrxml_pcdataelementtest.h"
#define INCLUDED_PCRXML_PCDATAELEMENTTEST
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
#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif

// Module headers.
#ifndef INCLUDED_PCRXML_DOMDIFF
#include "pcrxml_domdiff.h"
#define INCLUDED_PCRXML_DOMDIFF
#endif
#ifndef INCLUDED_PCRXML_DOCUMENT
#include "pcrxml_document.h"
#define INCLUDED_PCRXML_DOCUMENT
#endif
#ifndef INCLUDED_PCRGENXML_DATAENVELOP
#include "pcrgenxml_dataenvelop.h"
#define INCLUDED_PCRGENXML_DATAENVELOP
#endif


/*!
  \file
  This file contains the implementation of the PCDATAElementTest class.
*/

// NOTE use string failureExpected in files expected to fail, see style guide

//------------------------------------------------------------------------------
// DEFINITION OF STATIC PCDATAELEMENT MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*pcrxml::PCDATAElementTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<PCDATAElementTest> instance(new PCDATAElementTest());

  suite->add(BOOST_CLASS_TEST_CASE(&PCDATAElementTest::test, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF PCDATAELEMENT MEMBERS
//------------------------------------------------------------------------------

//! ctor
pcrxml::PCDATAElementTest::PCDATAElementTest()
{}

void pcrxml::PCDATAElementTest::test()
{
  struct Verify {
    // test sets either str or doc
    std::string  str;
    Document    *doc;
    QDomDocument textDoc,cdataDoc;
    DataEnvelop *s;

    Verify():
      doc(0),s(0)
    {};
    ~Verify() {
      delete s;
      delete doc;
    }
    void create() {
      if (str.size()) {
        s = new DataEnvelop(str);
        s->encoding = pcrxml::DataEnvelopEncoding::text;
      } else {
        PRECOND(doc);
        s = new DataEnvelop(doc->documentElement());
      }

      PRECOND(!s->asCDATASection());
      textDoc = s->toDomDocument();
      s->setAsCDATASection(true);
      cdataDoc = s->toDomDocument();
    }
    static std::string expected() {
       return "x &><\"' bla x";
    }

    bool testContents() const {
      return s->contents() == expected(); // "x bla bla x";
    }
    bool testCDATA() const {
     DomDiff diff(contentsAsString(cdataDoc),
     "<?xml version='1.0' encoding='ISO-8859-1' standalone='yes'?>"
     "<!DOCTYPE dataEnvelop PUBLIC '-//PCRaster//Generic' 'pcraster.dtd'>"
     "<dataEnvelop encoding='text' xmlns='http://www.pcraster.nl/xml'>"
     "<![CDATA[x &><\"' bla x]]></dataEnvelop>");
     return diff.equal(false);
    }
    bool testText() const {
     DomDiff diff(contentsAsString(textDoc),
     "<?xml version='1.0' encoding='ISO-8859-1' standalone='yes'?>"
     "<!DOCTYPE dataEnvelop PUBLIC '-//PCRaster//Generic' 'pcraster.dtd'>"
     "<dataEnvelop encoding='text' xmlns='http://www.pcraster.nl/xml'>"
     "x &amp;>&lt;&quot;' bla x</dataEnvelop>");
     return diff.equal(false);
    }
  };

  { // correct xml all in CDATA
    Verify v;
    v.doc = new Document(
           "<!DOCTYPE dataEnvelop> "
           " <dataEnvelop encoding='text'><![CDATA[x &><\"' bla x]]></dataEnvelop> ");
    v.create();
    BOOST_CHECK(v.testContents());
    BOOST_CHECK(v.testText());
    BOOST_CHECK(v.testCDATA());
  }
  { // itself is correct xml
    Verify v;
    v.str = v.expected();
    v.create();
    BOOST_CHECK(v.testContents());
    BOOST_CHECK(v.testText());
    BOOST_CHECK(v.testCDATA());
  }
  { // inner CDATA section
    Verify v;
    v.doc = new Document(
           "<!DOCTYPE dataEnvelop> "
           " <dataEnvelop encoding='text'>x <![CDATA[&><\"' bla x]]></dataEnvelop> ");
    v.create();
    BOOST_CHECK(v.testContents());
    BOOST_CHECK(v.testText());
    BOOST_CHECK(v.testCDATA());
  }
  { // inner CDATA section, some entitity encoding
    Verify v;
    v.doc = new Document(
           "<!DOCTYPE dataEnvelop> "
           " <dataEnvelop encoding='text'>x &amp;>&lt;<![CDATA[\"' bla x]]></dataEnvelop> ");
    v.create();
    BOOST_CHECK(v.testContents());
    BOOST_CHECK(v.testText());
    BOOST_CHECK(v.testCDATA());
  }
  { // all entitity encoding
    Verify v;
    v.doc = new Document(
           "<!DOCTYPE dataEnvelop> "
           " <dataEnvelop encoding='text'>x &amp;&gt;&lt;&quot;&apos; bla x</dataEnvelop> ");
    v.create();
    BOOST_CHECK(v.testContents());
    BOOST_CHECK(v.testText());
    BOOST_CHECK(v.testCDATA());
  }
  { // empty ok
    Verify v;
    v.doc = new Document(
           "<!DOCTYPE dataEnvelop> "
           " <dataEnvelop encoding='text'/> ");
    v.create();
    BOOST_CHECK(v.s->contents().empty());
  }
  { // mixed content model not allowed
   bool catchedExpectedException(false);
   try {
      Verify v;
      v.doc = new Document(
           "<!DOCTYPE dataEnvelop> "
           " <dataEnvelop encoding='text'> xx <InnerTag/></dataEnvelop> ");
      v.create();
   } catch (const com::BadStreamFormat& ) {
     catchedExpectedException = true;
   }
   BOOST_CHECK(catchedExpectedException);
  }
}
