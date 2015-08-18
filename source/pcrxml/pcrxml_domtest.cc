#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_PCRXML_DOMTEST
#include "pcrxml_domtest.h"
#define INCLUDED_PCRXML_DOMTEST
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
#ifndef INCLUDED_PCRXML_DOM
#include "pcrxml_dom.h"
#define INCLUDED_PCRXML_DOM
#endif
#ifndef INCLUDED_PCRXML_DOCUMENT
#include "pcrxml_document.h"
#define INCLUDED_PCRXML_DOCUMENT
#endif


/*!
  \file
  This file contains the implementation of the DomTest class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC DOM MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*pcrxml::DomTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<DomTest> instance(new DomTest());

  suite->add(BOOST_CLASS_TEST_CASE(&DomTest::testFirstMatchByTagName, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&DomTest::testChildrenByTagName, instance));

  suite->add(BOOST_CLASS_TEST_CASE(&DomTest::testChangeAttrName, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&DomTest::testTextOnlyContents, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&DomTest::testDomEquality, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF DOM MEMBERS
//------------------------------------------------------------------------------

//! ctor
pcrxml::DomTest::DomTest()
{
}



void pcrxml::DomTest::testFirstMatchByTagName()
{
  // this is the assumption made by firstMatchByTagName
  QDomElement d;
  BOOST_CHECK(d.isNull());

  {
   Document doc("<Main b='c'> ab<Na/>def <Na/></Main>");
   BOOST_CHECK(firstMatchByTagName(doc.documentElement(),"Nadda").isNull());
  }
  {
   Document doc("<Main b='c'> ab<Na AnAttr='x'/>def <Na/></Main>");
   QDomElement r=firstMatchByTagName(doc.documentElement(),"Na");
   BOOST_CHECK(!r.isNull());
   BOOST_CHECK(r.hasAttribute("AnAttr"));
  }
}

void pcrxml::DomTest::testChildrenByTagName()
{
  {
   Document doc("<Main b='c'> abdef </Main>");
   BOOST_CHECK(childrenByTagName(doc.documentElement(),"Na").empty());
   BOOST_CHECK(firstChildByTagName(doc.documentElement(),"Na").isNull());
  }
  {
   Document doc("<Main b='c'> abdef <Na/></Main>");
   BOOST_CHECK(childrenByTagName(doc.documentElement(),"Na").size()==1);
   BOOST_CHECK(!firstChildByTagName(doc.documentElement(),"Na").isNull());
  }
  {
   Document doc("<Main b='c'> ab<Na AnAttr='x'/>def <Na/></Main>");
   BOOST_CHECK(childrenByTagName(doc.documentElement(),"Na").size()==2);
   QDomElement r=firstMatchByTagName(doc.documentElement(),"Na");
   BOOST_CHECK(!r.isNull());
   BOOST_CHECK(r.hasAttribute("AnAttr"));
  }
}

void pcrxml::DomTest::testChangeAttrName()
{
 Document doc("<Main b='change'/>");
 QDomElement e(doc.documentElement());

 BOOST_CHECK(e.attribute("a").isNull());
 BOOST_CHECK(e.attribute("b")== "change");

 changeAttrName(e,"b","a");

 BOOST_CHECK(e.attribute("b").isNull());
 BOOST_CHECK(e.attribute("a")== "change");

 changeAttrName(e,"b","c");

 BOOST_CHECK(e.attribute("b").isNull());
 BOOST_CHECK(e.attribute("c").isNull());
 BOOST_CHECK(e.attribute("a")== "change");

}

void pcrxml::DomTest::testTextOnlyContents()
{
  {
   Document doc("<Main b='c'> abdef </Main>");
   BOOST_CHECK(textOnlyContents(doc.documentElement()) == " abdef ");
  }
  {
   Document doc("<Main b='c'></Main>");
   BOOST_CHECK(textOnlyContents(doc.documentElement()) == "");
  }
}

#ifndef INCLUDED_IOSTREAM
#include <iostream>
#define INCLUDED_IOSTREAM
#endif
/*!
 * generic testing of Qt Dom how to compare a DOM tree
 */
void pcrxml::DomTest::testDomEquality()
{
  { // exact the same
     DomDiff d("<Main b='c'>x<T/> </Main>",
               "<Main b='c'>x<T/> </Main>");
     BOOST_CHECK(d.equal(true));
  }
  { // white space only text nodes ignored
     DomDiff d("<Main b='c'>x<T/></Main>",
               "<Main  b='c'>x<T/>   </Main>");
     BOOST_CHECK(d.equal(true));
  }
  { // extra white space in text nodes found as difference
     DomDiff d("<Main b='c'>x<T/></Main>",
               "<Main  b='c'> x <T/></Main>");
     bool catched=false;
     try {
       d.equal(true);
     } catch(com::Exception& e) {
       BOOST_CHECK(e.messages().find("nodeValue") != std::string::npos);
       BOOST_CHECK(e.messages().find(" x ") != std::string::npos);
       catched=true;
     }
     BOOST_CHECK(catched);
  }
  {
     DomDiff d("<Main b='c'>x<T/> </Main>",
               "<Main b='c'><T/></Main>");

     bool catched=false;
     try {
       d.equal(true);
     } catch(com::Exception& e) {
       BOOST_CHECK(e.messages().find("nodeName") != std::string::npos);
       BOOST_CHECK(e.messages().find("<T/>") != std::string::npos);
       catched=true;
     }
     BOOST_CHECK(catched);
  }
  {
    // TEST on different namespace notation but the same
     DomDiff d("<Main xmlns:pcr1='hx' b='c'>x<pcr1:T/> </Main>",
               "<Main xmlns:pcr2='hx' b='c'>x<pcr2:T/> </Main>");
     BOOST_CHECK(!d.equal(false));
  }
  { // different nr of sub elements
     DomDiff d("<Main b='c'><TX/><T/></Main>",
               "<Main b='c'><TX/></Main>");
     BOOST_CHECK(!d.equal(false));
  }
}
