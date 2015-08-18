// #ifndef INCLUDED_STDDEFX
// #include "stddefx.h"
// #define INCLUDED_STDDEFX
// #endif

#ifndef INCLUDED_PCRXSD_XSDTEST
#include "pcrxsd_xsdtest.h"
#define INCLUDED_PCRXSD_XSDTEST
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
#ifndef INCLUDED_SSTREAM
#include <sstream>
#define INCLUDED_SSTREAM
#endif


// PCRaster library headers.
// Module headers.
#ifndef INCLUDED_PCRXSD_PCRASTERXSD
#include "PCRasterXSD.h"
#define INCLUDED_PCRXSD_PCRASTERXSD
#endif
#ifndef INCLUDED_PCRXSD_DOMINPUT
#include "pcrxsd_dominput.h"
#define INCLUDED_PCRXSD_DOMINPUT
#endif
/*!
 *  some tests to test/evaluate the xsd tool
 */

XERCES_CPP_NAMESPACE_USE

namespace pcrxsd {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC XSD MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*XsdTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<XsdTest> instance(new XsdTest());

  suite->add(BOOST_CLASS_TEST_CASE(&XsdTest::testValidation, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&XsdTest::testNoSchema, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&XsdTest::testXML2Class, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&XsdTest::testClass2XML, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF XSD MEMBERS
//------------------------------------------------------------------------------

//! ctor
XsdTest::XsdTest(
         )
{
}



//! setUp
void XsdTest::setUp()
{
}



//! tearDown
void XsdTest::tearDown()
{
}


void XsdTest::testXML2Class()
{

  // should not throw errors
  std::istringstream s("<definition name='a'                            \
            xmlns='http://www.pcraster.nl/pcrxml'                       \
            xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'           \
            xsi:schemaLocation='http://www.pcraster.nl/pcrxml PCRaster.xsd'>\
            <description><text>Haskell sucks</text></description>       \
            </definition>");
  std::auto_ptr<pcrxml::Definition> d(pcrxml::definition(s));

  std::string name(d->name());
  BOOST_CHECK(d->name()==name);
  BOOST_CHECK(!d->name().empty());

  BOOST_CHECK(!d->field());
  BOOST_CHECK(!d->relation());

  BOOST_CHECK(d->description());
  std::string des(d->description()->text());
  BOOST_CHECK(des=="Haskell sucks");
}

void XsdTest::testClass2XML()
{
  pcrxml::Definition d("nameValue");
  d.field(pcrxml::FieldValueOrType());

  std::ostringstream s;
  pcrxml::definition(s,d,pcrxsd::namespaceInfoMap("PCRaster.xsd"));
  BOOST_CHECK(true);
}

void XsdTest::testValidation()
{
  pcrxml::script("habitat1.xml");

  std::istringstream s("<pcr:definition name='a'                            \
            xmlns:pcr='http://www.pcraster.nl/pcrxml'                       \
            xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'           \
            xsi:schemaLocation='http://www.pcraster.nl/pcrxml PCRaster.xsd'/>");
  std::auto_ptr<pcrxml::Definition> d(pcrxml::definition(s));
  BOOST_CHECK(d->name()=="a");

  try {
    pcrxml::definition("fullNs.xml");
    pcrxml::definition("rootNs.xml");

    pcrxml::script("example1XmlReflection.xml");
    pcrxml::script("example2.xml");
    pcrxml::script("example2XmlReflection.xml");
    pcrxml::script("example2XmlReflectionAltered.xml");

    pcrxml::script("statistics.xml");
  } catch(Exception const& e) {
    // should not fail
    BOOST_CHECK_MESSAGE(false,e.msg());
  } catch(...) {
    BOOST_CHECK_MESSAGE(false,"Unknown exception");
  }
}

void XsdTest::testNoSchema()
{
 {/// no validate
  // set to schema PCRasterXX.xsd that should not be found
  std::string s(
    "<definition name='a'                                   \
      xmlns='http://www.pcraster.nl/pcrxml'                 \
      xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance' \
      xsi:schemaLocation='http://www.pcraster.nl/pcrxml PCRaster_X_X_X.xsd'/>");
  pcrxsd::DOMInput ip(DOMInput::CompiledIn);
  ip.setString(s);
  ip.setValidate(true);
  try {
    std::auto_ptr<pcrxml::Definition> d(pcrxml::definition(*ip.document()));
    std::string name("a");
    BOOST_CHECK(d->name()==name);
    BOOST_CHECK(!d->name().empty());
  } catch(Exception const& e) {
    // should not fail
    BOOST_CHECK_MESSAGE(false,e.msg());
  }
 }
 {/// no validate
  // set to schema PCRasterXX.xsd that should not be found
  std::istringstream s(
    "<definition name='a'                                   \
      xmlns='http://www.pcraster.nl/pcrxml'                 \
      xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance' \
      xsi:schemaLocation='http://www.pcraster.nl/pcrxml PCRaster_X_X_X.xsd'/>");
  std::auto_ptr<pcrxml::Definition> d(pcrxml::definition(s,
                                          xml_schema::flags::dont_validate));

  std::string name("a");
  BOOST_CHECK(d->name()==name);
  BOOST_CHECK(!d->name().empty());
 }
 {/// no validate
  // no schemaLocation
  std::istringstream s(
    "<definition name='a'                                   \
      xmlns='http://www.pcraster.nl/pcrxml'                 \
      xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'/>");
  std::auto_ptr<pcrxml::Definition> d(pcrxml::definition(s,
                                          xml_schema::flags::dont_validate));

  std::string name("a");
  BOOST_CHECK(d->name()==name);
  BOOST_CHECK(!d->name().empty());
 }
 {// validate, but do not find any schema, no xsd file
  std::istringstream s(
    "<definition name='a'                                   \
      xmlns='http://www.pcraster.nl/pcrxml'                 \
      xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance' \
      xsi:schemaLocation='http://www.pcraster.nl/pcrxml'/>");
  bool catched=false;
  try {
   std::auto_ptr<pcrxml::Definition> d(pcrxml::definition(s));
  } catch(...) {
    catched=true;
  }
  BOOST_CHECK(catched);
 }
 {// validate, find schema by property
  std::istringstream s(
    "<definition name='a'                                   \
      xmlns='http://www.pcraster.nl/pcrxml'                 \
      xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'/>");
  // xsi:schemaLocation='http://www.pcraster.nl/pcrxml'/>");

  xml_schema::properties props;
  props.schema_location (
      "http://www.pcraster.nl/pcrxml", // namespace
      "file:///home/cees/pcrtree/template/xml/PCRaster.xsd");
/*
  props.no_namespace_schema_location (
      "file:// /home/cees/pcrtree/template/xml/PCRaster.xsd");
 */

  bool noException(true);
  try {
   std::auto_ptr<pcrxml::Definition> d(pcrxml::definition(s,0,props));
   std::string name("a");
   BOOST_CHECK(d->name()==name);
   BOOST_CHECK(!d->name().empty());
  } catch (xml_schema::exception const& ) {
     noException=true;
  }
  BOOST_CHECK(noException);
 }
/*
 {// validate, but do not find any schema, no xsd file
  std::string s(
    "<definition name='a'                                   \
      xmlns='http://www.pcraster.nl/pcrxml'                 \
      xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'/>");

  pcrxsd::DOMInput ip(DOMInput::CompiledIn);
  ip.setString(s);
  ip.setValidate(false);
  bool noException=true;
  try {
   std::auto_ptr<pcrxml::Definition> d(pcrxml::definition(*ip.document()));
   BOOST_CHECK(d->name()=="a");
  } catch(...) {
    noException=false;
  }
  BOOST_CHECK(noException);
 }
*/
}


/*
Both in this chapter and the schema documentation the elements are explained in its IN and OUT meaning. These meanings are from the viewpoint of the API:
 * IN: how the element instructs the API when passed in by pcr_createScriptFromString()
 * OUT: what the element tells about the script definition when it is retrieved from pcr_ScriptXMLReflection()
*/

} // namespace pcrxsd

