#define BOOST_TEST_MODULE pcraster dal feature_path
#include <boost/test/unit_test.hpp>
#include "dal_Exception.h"
#include "dal_FeaturePath.h"


BOOST_AUTO_TEST_CASE(test_)
{
  using namespace dal;

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


BOOST_AUTO_TEST_CASE(compare)
{
  using namespace dal;

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


BOOST_AUTO_TEST_CASE(assignment)
{
  using namespace dal;

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


BOOST_AUTO_TEST_CASE(copy)
{
  using namespace dal;

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
