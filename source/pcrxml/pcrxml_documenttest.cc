#include "stddefx.h"
#include "pcrxml_documenttest.h"
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

#ifndef INCLUDED_QDOM
#include <qdom.h>
#define INCLUDED_QDOM
#endif

#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif
#ifndef INCLUDED_COM_PATHNAME
#include "com_pathname.h"
#define INCLUDED_COM_PATHNAME
#endif
#ifndef INCLUDED_COM_FILE
#include "com_file.h"
#define INCLUDED_COM_FILE
#endif
#ifndef INCLUDED_PCRXML_DOMDIFF
#include "pcrxml_domdiff.h"
#define INCLUDED_PCRXML_DOMDIFF
#endif
#ifndef INCLUDED_IOSTREAM
#include <iostream>
#define INCLUDED_IOSTREAM
#endif


/*!
  \file
  This file contains the implementation of the DocumentTest class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC CLASS MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*pcrxml::DocumentTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<DocumentTest> instance(new DocumentTest());

  suite->add(BOOST_CLASS_TEST_CASE(&DocumentTest::testPcrDocument, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&DocumentTest::testNotExistant, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&DocumentTest::testCtorAndParser, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&DocumentTest::testFirstMatchByTagName, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&DocumentTest::testNameSpaceStuff, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF CLASS MEMBERS
//------------------------------------------------------------------------------

//! ctor
pcrxml::DocumentTest::DocumentTest()
{
}

void pcrxml::DocumentTest::testPcrDocument()
{
  { // only document Element name
    Document doc(createPcrDocument("ExchangeModel"));
    DomDiff diff(doc.toStdString(),
     "<?xml version='1.0' encoding='ISO-8859-1' standalone='yes'?>"
     "<!DOCTYPE ExchangeModel PUBLIC '-//PCRaster//Generic' 'pcraster.dtd'>"
     " <ExchangeModel xmlns='http://www.pcraster.nl/xml'/>");
    BOOST_CHECK(diff.equal(false));
  }
  { // document Element 
    Document doc(createPcrDocument("<ExchangeModel/>"));
    DomDiff diff(doc.toStdString(),
     "<?xml version='1.0' encoding='ISO-8859-1' standalone='yes'?>"
     "<!DOCTYPE ExchangeModel PUBLIC '-//PCRaster//Generic' 'pcraster.dtd'>"
     " <ExchangeModel xmlns='http://www.pcraster.nl/xml'/>");
    BOOST_CHECK(diff.equal(false));
  }
  { // copy from element tree, do not add xmlns attrs
    Document doc(pcrxml::createPcrDocument(
          "<ExchangeModel xmlns='XX' attr='value'></ExchangeModel>"));

    const char *cmp= "<?xml version='1.0' encoding='ISO-8859-1' standalone='yes'?>"
     "<!DOCTYPE ExchangeModel PUBLIC '-//PCRaster//Generic' 'pcraster.dtd'>"
     "<ExchangeModel xmlns='http://www.pcraster.nl/xml' attr='value'/></ExchangeModel>";
    DomDiff diff(doc.toStdString(),cmp);
    BOOST_CHECK(diff.equal(false));
  }
  { // copy from element tree
    Document doc(pcrxml::createPcrDocument(
                 "<ExchangeModel><TT x='v'/><X>de</X></ExchangeModel>"));
    const char *cmp= "<?xml version='1.0' encoding='ISO-8859-1' standalone='yes'?>"
     "<!DOCTYPE ExchangeModel PUBLIC '-//PCRaster//Generic' 'pcraster.dtd'>"
     "<ExchangeModel xmlns='http://www.pcraster.nl/xml'><TT x='v'/><X>de</X></ExchangeModel>";
    DomDiff diff(doc.toStdString(),cmp);
    BOOST_CHECK(diff.equal(true));
  }
}

void pcrxml::DocumentTest::testNotExistant()
{
  bool catchedExpectedException(false);

  try {
    Document d(com::PathName("failureExpectedDoesNotExist.xml"));
  }
  catch(com::FileError const& ) {
    // According to the Document docs.
    catchedExpectedException = true;
  }

  BOOST_CHECK(catchedExpectedException);
}



//! test the Ctor and documentElement
void pcrxml::DocumentTest::testCtorAndParser()
{
 QDomNode el; // element document

 bool catchedExpectedException(false);
 try {
   //! no valid xml file
   Document invalidDoc("<licensedFeature value='EsriSupport'>");
 } catch (const com::BadStreamFormat& ) {
   catchedExpectedException = true;
 }
 BOOST_CHECK(catchedExpectedException);

 catchedExpectedException=false;
 try {
   com::PathName pn("empty.tmp"); // empty file, 0 bytes
   com::create(pn);
   //! no valid xml file
   Document invalidDoc(pn);
 } catch (const com::BadStreamFormat& ) {
   catchedExpectedException = true;
 }
 BOOST_CHECK(catchedExpectedException);

 catchedExpectedException=false;
 try {
   // not an XML file
   com::PathName pn("aNonXMLfile");
   //! no valid xml file
   Document invalidDoc(pn);
 } catch (const com::BadStreamFormat& ) {
   catchedExpectedException = true;
 }
 BOOST_CHECK(catchedExpectedException);

 try {
 Document docWithDocType("<licensedFeature value='EsriSupport' />");
 el = docWithDocType.documentElement();



 // tests finding of docType done with Xerces
 Document docWithNoDocType1("<licensedFeature value='EsriSupport' />");
 el = docWithNoDocType1.documentElement();

 Document docWithNoDocType2("<TestEmptyElementNoAttr/>");
 el = docWithNoDocType2.documentElement();

 Document docWithNoDocType3("<TestEmptyElementNoAttr	 />");
 el = docWithNoDocType3.documentElement();

 Document docWithNoDocType4("<TestEmptyElementNoAttr\n	 />");
 el = docWithNoDocType4.documentElement();

 Document docWithNoDocType5("<TestEmptyElementNoAttr></TestEmptyElementNoAttr>");
 el = docWithNoDocType5.documentElement();

 // ctor segfaulted on finding this docType
 Document doc6(
   "<systemLicense><licensee value='Piet Snot'/></systemLicense>");


 } catch (const com::Exception& ) {
   const bool expectNoExceptionsThrown(false);
   BOOST_CHECK(expectNoExceptionsThrown);
 }
}

//! test pcrxml::Document::firstMatchByTagName()
void pcrxml::DocumentTest::testFirstMatchByTagName()
{
 {
   Document head("<E1><E2/></E1>");
   QDomElement el=head.firstMatchByTagName("E1");
   BOOST_CHECK(!el.isNull());
   BOOST_CHECK(el.tagName()=="E1");
 }
 {
   Document selectFirst("<E1><E2 first='first'/><E2 second='second'/></E1>");
   QDomElement el=selectFirst.firstMatchByTagName("E2");
   BOOST_CHECK(!el.isNull());
   BOOST_CHECK(el.tagName()=="E2");
   BOOST_CHECK(el.hasAttribute("first"));
 }
 {
   Document selectFirst("<E1/>");
   QDomElement el=selectFirst.firstMatchByTagName("E2");
   BOOST_CHECK(el.isNull());
 }
}

void pcrxml::DocumentTest::testNameSpaceStuff()
{
}
