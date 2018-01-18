#define BOOST_TEST_MODULE pcraster xsd xsd
#include <boost/test/unit_test.hpp>
#include <sstream>
#include "PCRasterXSD.h"
#include "pcrxsd_dominput.h"
#include "pcrxsd_library.h"


XERCES_CPP_NAMESPACE_USE

using Fixture = pcrxsd::Library;

BOOST_GLOBAL_FIXTURE(Fixture);


BOOST_AUTO_TEST_CASE(xml_to_class)
{
  using namespace pcrxsd;

  // should not throw errors
  std::istringstream s("<definition name='a'                            \
            xmlns='http://www.pcraster.nl/pcrxml'                       \
            xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'           \
            xsi:schemaLocation='http://www.pcraster.nl/pcrxml PCRaster.xsd'>\
            <description><text>Haskell sucks</text></description>       \
            </definition>");
  std::unique_ptr<pcrxml::Definition> d(pcrxml::definition(s));

  std::string name(d->name());
  BOOST_CHECK(d->name()==name);
  BOOST_CHECK(!d->name().empty());

  BOOST_CHECK(!d->field());
  BOOST_CHECK(!d->relation());

  BOOST_CHECK(d->description());
  std::string des(d->description()->text());
  BOOST_CHECK(des=="Haskell sucks");
}


BOOST_AUTO_TEST_CASE(class_to_xml)
{
  using namespace pcrxsd;

  pcrxml::Definition d("nameValue");
  d.field(pcrxml::FieldValueOrType());

  std::ostringstream s;
  pcrxml::definition(s,d,pcrxsd::namespaceInfoMap("PCRaster.xsd"));
  BOOST_CHECK(true);
}


BOOST_AUTO_TEST_CASE(validation)
{
  using namespace pcrxsd;

  pcrxml::script("habitat1.xml");

  std::istringstream s("<pcr:definition name='a'                            \
            xmlns:pcr='http://www.pcraster.nl/pcrxml'                       \
            xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'           \
            xsi:schemaLocation='http://www.pcraster.nl/pcrxml PCRaster.xsd'/>");
  std::unique_ptr<pcrxml::Definition> d(pcrxml::definition(s));
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


BOOST_AUTO_TEST_CASE(no_schema)
{
  using namespace pcrxsd;

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
    std::unique_ptr<pcrxml::Definition> d(pcrxml::definition(*ip.document()));
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
  std::unique_ptr<pcrxml::Definition> d(pcrxml::definition(s,
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
  std::unique_ptr<pcrxml::Definition> d(pcrxml::definition(s,
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
   std::unique_ptr<pcrxml::Definition> d(pcrxml::definition(s));
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
   std::unique_ptr<pcrxml::Definition> d(pcrxml::definition(s,0,props));
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
