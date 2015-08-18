#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_PCRXML_DOMALGORITHMTEST
#include "pcrxml_domalgorithmtest.h"
#define INCLUDED_PCRXML_DOMALGORITHMTEST
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

// Module headers.
#ifndef INCLUDED_PCRXML_DOCUMENT
#include "pcrxml_document.h"
#define INCLUDED_PCRXML_DOCUMENT
#endif
#ifndef INCLUDED_PCRXML_DOMALGORITHM
#include "pcrxml_domalgorithm.h"
#define INCLUDED_PCRXML_DOMALGORITHM
#endif



/*!
  \file
  This file contains the implementation of the DomAlgorithmTest class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC DOMALGORITHM MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*pcrxml::DomAlgorithmTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<DomAlgorithmTest> instance(new DomAlgorithmTest());

  suite->add(BOOST_CLASS_TEST_CASE(&DomAlgorithmTest::testForEachNode, instance));

  suite->add(BOOST_CLASS_TEST_CASE(&DomAlgorithmTest::testForEachElement, instance));

  suite->add(BOOST_CLASS_TEST_CASE(&DomAlgorithmTest::testForEachChildElement, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF DOMALGORITHM MEMBERS
//------------------------------------------------------------------------------

//! ctor
pcrxml::DomAlgorithmTest::DomAlgorithmTest()
{
}

namespace pcrxml {
  //! count nodes that are attributes
  struct CountNodeAttrs {
    size_t nr;
    CountNodeAttrs():nr(0) {};
    void operator()(QDomNode n) {
      if (n.isAttr())
         nr++;
    }
  };
  struct ConcatNodeValues {
    QString val;
    ConcatNodeValues() {};
    void operator()(QDomNode n) {
      if (n.nodeValue() != QString::null)
        val += n.nodeValue();
    }
  };
  //! count nr of attributes of element
  struct CountAttrsOfElement {
    size_t nr;
    CountAttrsOfElement():nr(0) {};
    void operator()(QDomElement e) {
     nr+= e.attributes().count();
    }
  };
  struct ChangeTagName {
    QString d_oldName;
    QString d_newName;
    size_t nr;
    ChangeTagName(QString oldName, QString newName):
      d_oldName(oldName),d_newName(newName),nr(0){};
    void operator()(QDomElement e) {

      PRECOND(!e.isNull());
      if (e.tagName() == d_oldName) {
         e.setTagName(d_newName);
         nr++;
      }
    }
  };
}

void pcrxml::DomAlgorithmTest::testForEachNode()
{
  // ensure attr Node are processed in alfabetic order
  {
   ConcatNodeValues ca;
   Document doc("<Main><S1 />Text<S2 b='a2' a='a1'/><S3><S1 a='a3' /></S3></Main>");
   forEachNode(doc.documentElement(), ca);
   BOOST_CHECK(ca.val == "Texta1a2a3");
  }
  {
   ConcatNodeValues ca;
   Document doc("<Main><S1 />Text<S2 a='a1' b='a2'/><S3><S1 a='a3' /></S3></Main>");
   forEachNode(doc.documentElement(), ca);
   BOOST_CHECK(ca.val == "Texta1a2a3");
  }
}

void pcrxml::DomAlgorithmTest::testForEachElement()
{
 Document doc("<?jsx version='1'?><Main><S1 /><S2 b='a2'/><S3><S1 a='a2' /></S3></Main>");

 CountNodeAttrs ca;
 forEachElement(doc.documentElement(), ca);
 BOOST_CHECK(ca.nr==0);

 CountAttrsOfElement cae;
 forEachElement(doc.documentElement(), cae);
 BOOST_CHECK(cae.nr==2);

 ChangeTagName  ctn("S1","SX");
 forEachElement(doc.documentElement(), ctn);
 BOOST_CHECK(ctn.nr==2);
 BOOST_CHECK(doc.documentElement().tagName()=="Main");
 ChangeTagName  ctnAgain("S1","SX");
 forEachElement(doc.documentElement(), ctnAgain);
 BOOST_CHECK(ctnAgain.nr==0);

 ChangeTagName  ctnMain("Main","SX");
 forEachElement(doc.documentElement(), ctnMain);
 BOOST_CHECK(ctnMain.nr==1);
 BOOST_CHECK(doc.documentElement().tagName()=="SX");

 // the PI and the document element
 BOOST_CHECK(doc.childNodes().count() == 2);

}

void pcrxml::DomAlgorithmTest::testForEachChildElement()
{
 CountNodeAttrs ca;
 Document doc("<S1 b='notCountedAsChild'><S1 /><S2 b='a2'/><S3><S1 a='a2NotCountedAsChild' /></S3></S1>");
 forEachChildElement(doc.documentElement(), ca);
 BOOST_CHECK(ca.nr==0);

 // only S2 childs of root has attr
 CountAttrsOfElement ca2;
 forEachChildElement(doc.documentElement(), ca2);
 BOOST_CHECK(ca2.nr==1);

 // test changing
 ChangeTagName ctn("S1","SX");

 QDomElement e(doc.documentElement());
 forEachChildElement(e, ctn);
 BOOST_CHECK(ctn.nr==1);
 BOOST_CHECK(e.tagName()=="S1");
 forEachChildElement(e, ctn);
 BOOST_CHECK(ctn.nr==1);
}
