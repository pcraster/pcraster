#define BOOST_TEST_MODULE pcraster pcrxml pcdata_element
#include <boost/test/unit_test.hpp>
#include "com_exception.h"
#include "pcrxml_domdiff.h"
#include "pcrxml_document.h"
#include "pcrgenxml_dataenvelop.h"


BOOST_AUTO_TEST_CASE(test)
{
  using namespace pcrxml;

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
