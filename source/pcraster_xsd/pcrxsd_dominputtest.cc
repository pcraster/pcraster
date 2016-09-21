#define BOOST_TEST_MODULE pcraster xsd dom_input
#include <boost/test/unit_test.hpp>
#include <xercesc/dom/DOMDocument.hpp>
#include <xercesc/dom/DOMElement.hpp>
#include "pcrxsd_dominput.h"
#include "pcrxsd_library.h"
#include "pcrxsd_utils.h"


using Fixture = pcrxsd::Library;

BOOST_GLOBAL_FIXTURE(Fixture);

BOOST_AUTO_TEST_CASE(validate)
{
  using namespace pcrxsd;

  {
    DOMInput di;
    di.setValidate(true);
    di.setFile("notPCRasterXSDConforming.xml");
    bool catched=false;
    try {
      di.document();
    } catch(Exception const&) {
      catched=true;
    }
    BOOST_CHECK(catched);
  }
  {
    DOMInput di;
    di.setValidate(false);
    di.setFile("notPCRasterXSDConforming.xml");
    di.document();
  }
  {
    DOMInput di;
    di.setValidate(true);
    di.setFile("habitat1.xml");
    di.document();
  }
}


BOOST_AUTO_TEST_CASE(entity_resolver)
{
  using namespace pcrxsd;

 { // catch the external entity PCRaster_X_X_X.xsd
  std::string s(
    "<definition name='a'                                   \
      xmlns='http://www.pcraster.nl/pcrxml'                 \
      xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance' \
      xsi:schemaLocation='http://www.pcraster.nl/pcrxml PCRaster_X_X_X.xsd'/>");
  DOMInput di(DOMInput::CompiledIn);
  di.setString(s);
  di.setValidate(false);
  try {
  di.document();
  } catch(Exception const& e) {
    // should not fail
    BOOST_CHECK_MESSAGE(false,e.msg());
  }
 }
 { // catch the external entity PCRaster_X_X_X.xsd and validation error
  std::string s(
    "<definition namexxx='a'                                \
      xmlns='http://www.pcraster.nl/pcrxml'                 \
      xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance' \
      xsi:schemaLocation='http://www.pcraster.nl/pcrxml PCRaster_X_X_X.xsd'/>");
  DOMInput di(DOMInput::CompiledIn);
  di.setString(s);
  di.setValidate(true);
  try {
   di.document();
  } catch(Exception const& e) {
    BOOST_CHECK(e.msg().find("namexxx") != std::string::npos);
  }
 }
}


BOOST_AUTO_TEST_CASE(not_well_formed)
{
  using namespace pcrxsd;

  DOMInput di;
  di.setValidate(true);
  di.setFile("notWellFormed.xml");
  bool catched=false;
  try {
    di.document();
  } catch(Exception const& ) {
    catched=true;
  }
  BOOST_CHECK(catched);
}
