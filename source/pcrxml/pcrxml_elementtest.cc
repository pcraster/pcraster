#define BOOST_TEST_MODULE pcraster pcrxml element
#include <boost/test/unit_test.hpp>
#include "pcrgenxml_visualisationconfiguration.h"
#include "pcrgenxml_data.h"
#include "pcrxml_document.h"
#include <qdom.h>
#include "pcrxml_ostream.h"
#include <sstream>
#include "com_pathinfo.h"
#include "com_exception.h"


//! test write and writeToFile method
BOOST_AUTO_TEST_CASE(write_to_file)
{
  using namespace pcrxml;

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
BOOST_AUTO_TEST_CASE(to_dom_document)
{
  // using namespace pcrxml;
}


BOOST_AUTO_TEST_CASE(required)
{
  using namespace pcrxml;

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
