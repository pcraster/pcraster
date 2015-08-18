// #ifndef INCLUDED_STDDEFX
// #include "stddefx.h"
// #define INCLUDED_STDDEFX
// #endif

#ifndef INCLUDED_PCRXSD_DOMALGORITHMTEST
#include "pcrxsd_DomAlgorithmTest.h"
#define INCLUDED_PCRXSD_DOMALGORITHMTEST
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

#include <xercesc/dom/DOMDocument.hpp>


// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_PCRXSD_DOMALGORITHM
#include "pcrxsd_domalgorithm.h"
#define INCLUDED_PCRXSD_DOMALGORITHM
#endif
#ifndef INCLUDED_PCRXSD_UTILS
#include "pcrxsd_utils.h"
#define INCLUDED_PCRXSD_UTILS
#endif
#ifndef INCLUDED_PCRXSD_STRINGDOMINPUT
#include "pcrxsd_StringDomInput.h"
#define INCLUDED_PCRXSD_STRINGDOMINPUT
#endif
/*!
  \file
  This file contains the implementation of the DomAlgorithmTest class.
*/

XERCES_CPP_NAMESPACE_USE

namespace pcrxsd {


//------------------------------------------------------------------------------
// DEFINITION OF STATIC DOMALGORITHMTEST MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite* DomAlgorithmTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<DomAlgorithmTest> instance(new DomAlgorithmTest());
  suite->add(BOOST_CLASS_TEST_CASE(&DomAlgorithmTest::testForEachNode, instance));
  //  suite->add(BOOST_CLASS_TEST_CASE(&DomAlgorithmTest::testForEachElement, instance));
  // suite->add(BOOST_CLASS_TEST_CASE(&DomAlgorithmTest::testForEachChildElement, instance));


  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF DOMALGORITHMTEST MEMBERS
//------------------------------------------------------------------------------

//! ctor
DomAlgorithmTest::DomAlgorithmTest()
{
}



// TODO
typedef std::string QString;

//! count nodes that are attributes
struct CountNodeAttrs {
  size_t nr;
  CountNodeAttrs():nr(0) {};
  void operator()(DOMNode* n) {
    if (n->getNodeType() == DOMNode::ATTRIBUTE_NODE)
       nr++;
  }
};
struct ConcatNodeValues {
  QString val;
  ConcatNodeValues() {};
  void operator()(DOMNode* n) {
    if (n->getNodeValue() != 0)
      val += toString(n->getNodeValue());
  }
};
//! count nr of attributes of element
struct CountAttrsOfElement {
  size_t nr;
  CountAttrsOfElement():nr(0) {};
  void operator()(DOMElement* e) {
   nr+= e->getAttributes()->getLength();
  }
};

/* no setTagName in XERCES DOM
struct ChangeTagName {
  QString d_oldName;
  QString d_newName;
  size_t nr;
  ChangeTagName(QString oldName, QString newName):
    d_oldName(oldName),d_newName(newName),nr(0){};
  void operator()(DOMElement* e) {

    assert(!e->getNodeValue());
    if (e->getTagName() == d_oldName) {
       e->setTagName(d_newName);
       nr++;
    }
  }
};
*/

void pcrxsd::DomAlgorithmTest::testForEachNode()
{
  // ensure attr Node are processed in alfabetic order
  {
   ConcatNodeValues ca;

   StringDomInput sdi("<Main><S1 />Text<S2 b='a2' a='a1'/><S3><S1 a='a3' /></S3></Main>");
   DOMDocument* doc(sdi.document());
   BOOST_REQUIRE(doc);
   pcrxml::forEachNode(doc->getDocumentElement(), ca);
   BOOST_CHECK(ca.val == "Texta1a2a3");
  }
}

void pcrxsd::DomAlgorithmTest::testForEachElement()
{
/*
 Document doc("<?jsx version='1'?><Main><S1 /><S2 b='a2'/><S3><S1 a='a2' /></S3></Main>");

 CountNodeAttrs ca;
 forEachElement(doc->getDocumentElement(), ca);
 BOOST_CHECK(ca.nr==0);

 CountAttrsOfElement cae;
 forEachElement(doc->getDocumentElement(), cae);
 BOOST_CHECK(cae.nr==2);

 ChangeTagName  ctn("S1","SX");
 forEachElement(doc->getDocumentElement(), ctn);
 BOOST_CHECK(ctn.nr==2);
 BOOST_CHECK(doc->getDocumentElement().tagName()=="Main");
 ChangeTagName  ctnAgain("S1","SX");
 forEachElement(doc->getDocumentElement(), ctnAgain);
 BOOST_CHECK(ctnAgain.nr==0);

 ChangeTagName  ctnMain("Main","SX");
 forEachElement(doc->getDocumentElement(), ctnMain);
 BOOST_CHECK(ctnMain.nr==1);
 BOOST_CHECK(doc->getDocumentElement().tagName()=="SX");

 // the PI and the document element
 BOOST_CHECK(doc.childNodes().count() == 2);
*/
}

void pcrxsd::DomAlgorithmTest::testForEachChildElement()
{
/*
 CountNodeAttrs ca;
 Document doc("<S1 b='notCountedAsChild'><S1 /><S2 b='a2'/><S3><S1 a='a2NotCountedAsChild' /></S3></S1>");
 forEachChildElement(doc->getDocumentElement(), ca);
 BOOST_CHECK(ca.nr==0);

 // only S2 childs of root has attr
 CountAttrsOfElement ca2;
 forEachChildElement(doc->getDocumentElement(), ca2);
 BOOST_CHECK(ca2.nr==1);

// NOT SUPPORTED
//  // test changing
//  ChangeTagName ctn("S1","SX");
//
//  DOMElement* e(doc->getDocumentElement());
//  forEachChildElement(e, ctn);
//  BOOST_CHECK(ctn.nr==1);
//  BOOST_CHECK(e.tagName()=="S1");
//  forEachChildElement(e, ctn);
//  BOOST_CHECK(ctn.nr==1);
*/
}

} // namespace pcrxsd
