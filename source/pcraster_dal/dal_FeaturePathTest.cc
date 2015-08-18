#ifndef INCLUDED_DAL_FEATUREPATHTEST
#include "dal_FeaturePathTest.h"
#define INCLUDED_DAL_FEATUREPATHTEST
#endif

// External headers.
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

// Project headers.

// Module headers.
#ifndef INCLUDED_DAL_EXCEPTION
#include "dal_Exception.h"
#define INCLUDED_DAL_EXCEPTION
#endif

#ifndef INCLUDED_DAL_FEATUREPATH
#include "dal_FeaturePath.h"
#define INCLUDED_DAL_FEATUREPATH
#endif



/*!
  \file
  This file contains the implementation of the FeaturePathTest class.
*/



namespace dal {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC FEATUREPATHTEST MEMBERS
//------------------------------------------------------------------------------

//! Suite.
boost::unit_test::test_suite* FeaturePathTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<FeaturePathTest> instance(
         new FeaturePathTest());
  suite->add(BOOST_CLASS_TEST_CASE(
         &FeaturePathTest::test, instance));
  suite->add(BOOST_CLASS_TEST_CASE(
         &FeaturePathTest::testCompare, instance));
  suite->add(BOOST_CLASS_TEST_CASE(
         &FeaturePathTest::testAssignment, instance));
  suite->add(BOOST_CLASS_TEST_CASE(
         &FeaturePathTest::testCopy, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF FEATUREPATHTEST MEMBERS
//------------------------------------------------------------------------------

//! Constructor.
FeaturePathTest::FeaturePathTest()
{
}



void FeaturePathTest::test()
{
  {
    FeaturePath path;
    BOOST_CHECK(!path.isValid());
  }

  {
    FeaturePath path("source", FeaturePath::WithAttribute);
    BOOST_CHECK(!path.isValid());
  }

  {
    FeaturePath path("source/layer", FeaturePath::WithAttribute);
    BOOST_CHECK(!path.isValid());
  }

  {
    FeaturePath path("source/layer", FeaturePath::WithoutAttribute);
    BOOST_CHECK(path.isValid());
    BOOST_CHECK_EQUAL(path.source(), "source");
    BOOST_CHECK_EQUAL(path.layer(), "layer");
    BOOST_CHECK(path.attribute().empty());
  }

  {
    FeaturePath path("source/layer/attribute", FeaturePath::WithAttribute);
    BOOST_CHECK(path.isValid());
    BOOST_CHECK_EQUAL(path.source(), "source");
    BOOST_CHECK_EQUAL(path.layer(), "layer");
    BOOST_CHECK_EQUAL(path.attribute(), "attribute");
  }

  {
    FeaturePath path("blabla/source/layer", FeaturePath::WithoutAttribute);
    BOOST_CHECK(path.isValid());
    BOOST_CHECK_EQUAL(path.source(), "blabla/source");
    BOOST_CHECK_EQUAL(path.layer(), "layer");
    BOOST_CHECK(path.attribute().empty());
  }

  {
    FeaturePath path("source//attribute", FeaturePath::WithAttribute);
    BOOST_CHECK(!path.isValid());
  }

  {
    FeaturePath path("source/layer/", FeaturePath::WithoutAttribute);
    BOOST_REQUIRE(path.isValid());
    BOOST_CHECK_EQUAL(path.source(), "source");
    BOOST_CHECK_EQUAL(path.layer(), "layer");
    BOOST_CHECK(path.attribute().empty());
  }

  {
    FeaturePath path("c:/Program Files/source/layer/attribute",
         FeaturePath::WithAttribute);
    BOOST_CHECK(path.isValid());
    BOOST_CHECK_EQUAL(path.source(), "c:/Program Files/source");
    BOOST_CHECK_EQUAL(path.layer(), "layer");
    BOOST_CHECK_EQUAL(path.attribute(), "attribute");
  }

  {
    FeaturePath path("c:/Program Files/source/layer",
         FeaturePath::WithoutAttribute);
    BOOST_CHECK(path.isValid());
    BOOST_CHECK_EQUAL(path.source(), "c:/Program Files/source");
    BOOST_CHECK_EQUAL(path.layer(), "layer");
    BOOST_CHECK(path.attribute().empty());
  }

  {
    FeaturePath path("./lines",
         FeaturePath::WithoutAttribute);
    BOOST_CHECK(path.isValid());
    BOOST_CHECK_EQUAL(path.source(), ".");
    BOOST_CHECK_EQUAL(path.layer(), "lines");
    BOOST_CHECK(path.attribute().empty());
  }

  {
    FeaturePath path("./lines/attribute",
         FeaturePath::WithAttribute);
    BOOST_CHECK(path.isValid());
    BOOST_CHECK_EQUAL(path.source(), ".");
    BOOST_CHECK_EQUAL(path.layer(), "lines");
    BOOST_CHECK_EQUAL(path.attribute(), "attribute");
  }
}



void FeaturePathTest::testCompare()
{
  {
    FeaturePath path1, path2;
    BOOST_CHECK(path1 == path2);
  }

  {
    FeaturePath path1("c:/Program Files/source/layer/attribute",
         FeaturePath::WithAttribute);
    FeaturePath path2("c:/Program Files/source/layer/attribute",
         FeaturePath::WithAttribute);
    BOOST_CHECK(path1 == path2);
  }
}



void FeaturePathTest::testAssignment()
{
  {
    FeaturePath path1;
    FeaturePath path2 = path1;
    BOOST_CHECK(path1 == path2);
  }

  {
    FeaturePath path1("c:/Program Files/source/layer/attribute",
         FeaturePath::WithAttribute);
    FeaturePath path2 = path1;
    BOOST_CHECK(path1 == path2);
  }
}



void FeaturePathTest::testCopy()
{
  {
    FeaturePath path1;
    FeaturePath path2(path1);
    BOOST_CHECK(path1 == path2);
  }

  {
    FeaturePath path1("c:/Program Files/source/layer/attribute",
         FeaturePath::WithAttribute);
    FeaturePath path2(path1);
    BOOST_CHECK(path1 == path2);
  }
}

} // namespace dal

