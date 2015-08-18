#include "stddefx.h"
#include "pcrxml_elementtest.h"

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

#ifndef INCLUDED_PCRGENXML_VISUALISATIONCONFIGURATION
#include "pcrgenxml_visualisationconfiguration.h"
#define INCLUDED_PCRGENXML_VISUALISATIONCONFIGURATION
#endif
#ifndef INCLUDED_PCRGENXML_DATA
#include "pcrgenxml_data.h"
#define INCLUDED_PCRGENXML_DATA
#endif


#ifndef INCLUDED_PCRXML_DOCUMENT
#include "pcrxml_document.h"
#define INCLUDED_PCRXML_DOCUMENT
#endif

#ifndef  INCLUDED_QDOM
#include <qdom.h>
#define  INCLUDED_QDOM
#endif

#ifndef INCLUDED_PCRXML_OSTREAM
#include "pcrxml_ostream.h"
#define INCLUDED_PCRXML_OSTREAM
#endif

#ifndef INCLUDED_SSTREAM
#include <sstream>
#define INCLUDED_SSTREAM
#endif

#ifndef INCLUDED_COM_PATHINFO
#include "com_pathinfo.h"
#define INCLUDED_COM_PATHINFO
#endif

#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif

/*!
  \file
  This file contains the implementation of the ElementTest class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC CLASS MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*pcrxml::ElementTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<ElementTest> instance(new ElementTest());

  suite->add(BOOST_CLASS_TEST_CASE(&ElementTest::testToDomDocument, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&ElementTest::testWriteToFile, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&ElementTest::testRequired, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF CLASS MEMBERS
//------------------------------------------------------------------------------

//! ctor
pcrxml::ElementTest::ElementTest()
{
}

//! test write and writeToFile method
void pcrxml::ElementTest::testWriteToFile()
{
 { // do write
  pcrxml::VisualisationConfiguration vc;

  vc.date    = "independence day";
  vc.version = "app version";
  vc.cwd     = "GNUPH";
  vc.os      = pcrxml::RuntimePlatform::RP_LINUX;

  VisualisationGroup *gr = new VisualisationGroup();
  gr->dataObject = new DataObject();
  gr->dataObject->cursor = new Cursor();
  gr->dataObject->cursor->x = 1;
  gr->dataObject->cursor->y = 2;
  gr->dataObject->cursor->t = 3;
  vc.visualisationGroup.push_back(gr);

  vc.writeToFile("test2.xml");
 }
}

//! test both toDomDocument and appendTo method
/*!
 * NOTE did not work with ostringstream on gcc 2.96 !
 */
void pcrxml::ElementTest::testToDomDocument()
{
}
void pcrxml::ElementTest::testRequired()
{
 { // misses required attribute name
  bool catched(false);
  try {
   Document head("<Data/>");
   pcrxml::Data dt(head.documentElement());
  } catch (const com::Exception& e) {
    BOOST_CHECK(e.messages().find(
          "ttribute 'name' as part of element Data") != std::string::npos);
    catched=true;
  }
  BOOST_CHECK(catched);
 }
 { // wrong enum/NMTOKEN
  bool catched(false);
  try {
   Document head("<Data name='x' ioType='y' />");
   pcrxml::Data dt(head.documentElement());
  } catch (const com::Exception& e) {
    BOOST_CHECK(e.messages().find(
          "attribute ioType: y is not a defined NMTOKEN value") != std::string::npos);
    catched=true;
  }
  BOOST_CHECK(catched);
 }
 {
   pcrxml::Data dt;
   // dt.name not present while required
   BOOST_CHECK(!dt.name.present());

   pcrxml::Data dtCopy(dt);
   BOOST_CHECK(!dtCopy.name.present());
 }
 { // Can skip unknown attrs, what we knwo see as
   // a feature to hack up Schema support
  try {
   Document head("<Data "
       "xsi:noNamespaceSchemaLocation='ExchangeModel.xsd'"
       "xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'"
       "name='x' ioType='Input' />");
   pcrxml::Data dt(head.documentElement());
   BOOST_CHECK(dt.name()=="x");
  } catch (const com::Exception& e) {
    PRINT_VAR(e.messages());
    BOOST_CHECK(e.messages().find( "BLA BLA") != std::string::npos);
  }
 }
 { // misses required element (Map|...|Table)
  bool doesNotTellRequiredChoiseIsMissing(false);
  try {
   Document head("<Data name='x' ioType='Input' />");
   pcrxml::Data dt(head.documentElement());
  } catch (const com::Exception& e) {
    PRINT_VAR(e.messages());
    BOOST_CHECK(e.messages().find( "BLA BLA") != std::string::npos);
    doesNotTellRequiredChoiseIsMissing=true;
  }
  BOOST_WARN(doesNotTellRequiredChoiseIsMissing);
 }

}
