#define BOOST_TEST_MODULE pcraster pcrxml document
#include <boost/test/unit_test.hpp>
#include "com_exception.h"
#include "com_file.h"
#include "com_pathname.h"
#include "pcrxml_document.h"
#include "pcrxml_domdiff.h"
#include <qdom.h>


BOOST_AUTO_TEST_CASE(pcr_document)
{
  using namespace pcrxml;

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


BOOST_AUTO_TEST_CASE(non_existant)
{
  using namespace pcrxml;

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



// test the Ctor and documentElement
BOOST_AUTO_TEST_CASE(ctor_and_parser)
{
  using namespace pcrxml;

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
BOOST_AUTO_TEST_CASE(first_match_by_tag_name)
{
  using namespace pcrxml;

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


BOOST_AUTO_TEST_CASE(namespace_stuff)
{
  // using namespace pcrxml;
}
