#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_OBJECTLINKMETATEST
#include "calc_objectlinkmetatest.h"
#define INCLUDED_CALC_OBJECTLINKMETATEST
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
#ifndef INCLUDED_CALC_OBJECTLINKMETA
#include "calc_objectlinkmeta.h"
#define INCLUDED_CALC_OBJECTLINKMETA
#endif



/*!
  \file
  This file contains the implementation of the ObjectLinkMetaTest class.
*/

// NOTE use string failureExpected in files expected to fail, see style guide

//------------------------------------------------------------------------------
// DEFINITION OF STATIC OBJECTLINKMETA MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*calc::ObjectLinkMetaTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<ObjectLinkMetaTest> instance(new ObjectLinkMetaTest());

  suite->add(BOOST_CLASS_TEST_CASE(&ObjectLinkMetaTest::testCtor, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF OBJECTLINKMETA MEMBERS
//------------------------------------------------------------------------------

//! ctor
calc::ObjectLinkMetaTest::ObjectLinkMetaTest()
{
}



//! setUp
void calc::ObjectLinkMetaTest::setUp()
{
}



//! tearDown
void calc::ObjectLinkMetaTest::tearDown()
{
}



void calc::ObjectLinkMetaTest::testCtor()
{
  ObjectLinkMeta olm("mldd",0);
  //! ctor
  olm.pushBack("",false,VS_STRING,ST_NON);
  olm.pushBack("",false,VS_S,ST_EITHER);
  //! method
  olm.pushBack("twoInputOneResult",false,VS_N,ST_SPATIAL);
  olm.pushBack("twoInputOneResult",false,VS_S,ST_EITHER);
  olm.pushBack("twoInputOneResult",true, VS_O,ST_SPATIAL);

  BOOST_CHECK(olm.d_methods.size()==2);
  //! ctor
  ObjectLinkMeta::MethodMap::iterator m=olm.d_methods.find("");
  BOOST_CHECK(m!=olm.d_methods.end());
  BOOST_CHECK(m->second.d_name.empty());
  BOOST_CHECK(m->second.d_input.size()==2);
  BOOST_CHECK(m->second.d_input[0].vs==VS_STRING);
  BOOST_CHECK(m->second.d_input[0].st==ST_NON);
  BOOST_CHECK(m->second.d_input[1].vs==VS_S);
  BOOST_CHECK(m->second.d_input[1].st==ST_EITHER);
  BOOST_CHECK(m->second.d_result.size()==1);
  BOOST_CHECK(m->second.d_result[0].vs==VS_OBJECT);

  //! twoInputOneResult
  m=olm.d_methods.find("twoInputOneResult");
  BOOST_CHECK(m!=olm.d_methods.end());
  BOOST_CHECK(m->second.d_name=="twoInputOneResult");
  BOOST_CHECK(m->second.d_input.size()==2);
  BOOST_CHECK(m->second.d_input[0].vs==VS_N);
  BOOST_CHECK(m->second.d_input[0].st==ST_SPATIAL);
  BOOST_CHECK(m->second.d_input[1].vs==VS_S);
  BOOST_CHECK(m->second.d_input[1].st==ST_EITHER);
  BOOST_CHECK(m->second.d_result.size()==1);
  BOOST_CHECK(m->second.d_result[0].vs==VS_O);
  BOOST_CHECK(m->second.d_result[0].st==ST_SPATIAL);

}
