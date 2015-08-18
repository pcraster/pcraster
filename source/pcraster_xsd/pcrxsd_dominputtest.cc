#ifndef INCLUDED_PCRXSD_DOMINPUTTEST
#include "pcrxsd_dominputtest.h"
#define INCLUDED_PCRXSD_DOMINPUTTEST
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
#include <xercesc/dom/DOMElement.hpp>

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_PCRXSD_DOMINPUT
#include "pcrxsd_dominput.h"
#define INCLUDED_PCRXSD_DOMINPUT
#endif
#ifndef INCLUDED_PCRXSD_UTILS
#include "pcrxsd_utils.h"
#define INCLUDED_PCRXSD_UTILS
#endif


/*!
  \file
  This file contains the implementation of the DOMInputTest class.
*/

// NOTE use string failureExpected in files expected to fail, see style guide



namespace pcrxsd {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC DOMINPUT MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*DOMInputTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<DOMInputTest> instance(new DOMInputTest());

  suite->add(BOOST_CLASS_TEST_CASE(&DOMInputTest::testValidate, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&DOMInputTest::testNotWellFormed, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&DOMInputTest::testEntityResolver, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF DOMINPUT MEMBERS
//------------------------------------------------------------------------------

//! ctor
DOMInputTest::DOMInputTest(
         )
{
}



//! setUp
void DOMInputTest::setUp()
{
}



//! tearDown
void DOMInputTest::tearDown()
{
}



void DOMInputTest::testValidate()
{
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

void DOMInputTest::testEntityResolver()
{
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

void DOMInputTest::testNotWellFormed()
{
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

} // namespace pcrxsd
