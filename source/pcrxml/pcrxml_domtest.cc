#define BOOST_TEST_MODULE pcraster pcrxml dom
#include <boost/test/unit_test.hpp>
#include "com_exception.h"
#include "pcrxml_domdiff.h"
#include "pcrxml_dom.h"
#include "pcrxml_document.h"


BOOST_AUTO_TEST_CASE(first_match_by_tag_name)
{
  using namespace pcrxml;

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


BOOST_AUTO_TEST_CASE(children_by_tag_name)
{
  using namespace pcrxml;

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


BOOST_AUTO_TEST_CASE(change_attr_name)
{
  using namespace pcrxml;

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


BOOST_AUTO_TEST_CASE(text_only_contents)
{
  using namespace pcrxml;

  {
   Document doc("<Main b='c'> abdef </Main>");
   BOOST_CHECK(textOnlyContents(doc.documentElement()) == " abdef ");
  }
  {
   Document doc("<Main b='c'></Main>");
   BOOST_CHECK(textOnlyContents(doc.documentElement()) == "");
  }
}


/*!
 * generic testing of Qt Dom how to compare a DOM tree
 */
BOOST_AUTO_TEST_CASE(dom_equality)
{
  using namespace pcrxml;

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
