#define BOOST_TEST_MODULE pcraster dal feature_path
#include <boost/test/unit_test.hpp>
#include "dal_Exception.h"
#include "dal_FeaturePath.h"


BOOST_AUTO_TEST_CASE(test_)
{
  using namespace dal;

  {
    FeaturePath const path;
    BOOST_TEST(!path.isValid());
  }

  {
    FeaturePath const path("source", FeaturePath::WithAttribute);
    BOOST_TEST(!path.isValid());
  }

  {
    FeaturePath const path("source/layer", FeaturePath::WithAttribute);
    BOOST_TEST(!path.isValid());
  }

  {
    FeaturePath const path("source/layer", FeaturePath::WithoutAttribute);
    BOOST_TEST(path.isValid());
    BOOST_CHECK_EQUAL(path.source(), "source");
    BOOST_CHECK_EQUAL(path.layer(), "layer");
    BOOST_TEST(path.attribute().empty());
  }

  {
    FeaturePath const path("source/layer/attribute", FeaturePath::WithAttribute);
    BOOST_TEST(path.isValid());
    BOOST_CHECK_EQUAL(path.source(), "source");
    BOOST_CHECK_EQUAL(path.layer(), "layer");
    BOOST_CHECK_EQUAL(path.attribute(), "attribute");
  }

  {
    FeaturePath const path("blabla/source/layer", FeaturePath::WithoutAttribute);
    BOOST_TEST(path.isValid());
    BOOST_CHECK_EQUAL(path.source(), "blabla/source");
    BOOST_CHECK_EQUAL(path.layer(), "layer");
    BOOST_TEST(path.attribute().empty());
  }

  {
    FeaturePath const path("source//attribute", FeaturePath::WithAttribute);
    BOOST_TEST(!path.isValid());
  }

  {
    FeaturePath const path("source/layer/", FeaturePath::WithoutAttribute);
    BOOST_TEST_REQUIRE(path.isValid());
    BOOST_CHECK_EQUAL(path.source(), "source");
    BOOST_CHECK_EQUAL(path.layer(), "layer");
    BOOST_TEST(path.attribute().empty());
  }

  {
    FeaturePath const path("c:/Program Files/source/layer/attribute",
         FeaturePath::WithAttribute);
    BOOST_TEST(path.isValid());
    BOOST_CHECK_EQUAL(path.source(), "c:/Program Files/source");
    BOOST_CHECK_EQUAL(path.layer(), "layer");
    BOOST_CHECK_EQUAL(path.attribute(), "attribute");
  }

  {
    FeaturePath const path("c:/Program Files/source/layer",
         FeaturePath::WithoutAttribute);
    BOOST_TEST(path.isValid());
    BOOST_CHECK_EQUAL(path.source(), "c:/Program Files/source");
    BOOST_CHECK_EQUAL(path.layer(), "layer");
    BOOST_TEST(path.attribute().empty());
  }

  {
    FeaturePath const path("./lines",
         FeaturePath::WithoutAttribute);
    BOOST_TEST(path.isValid());
    BOOST_CHECK_EQUAL(path.source(), ".");
    BOOST_CHECK_EQUAL(path.layer(), "lines");
    BOOST_TEST(path.attribute().empty());
  }

  {
    FeaturePath const path("./lines/attribute",
         FeaturePath::WithAttribute);
    BOOST_TEST(path.isValid());
    BOOST_CHECK_EQUAL(path.source(), ".");
    BOOST_CHECK_EQUAL(path.layer(), "lines");
    BOOST_CHECK_EQUAL(path.attribute(), "attribute");
  }
}


BOOST_AUTO_TEST_CASE(compare)
{
  using namespace dal;

  {
    FeaturePath const path1;
    FeaturePath const path2;
    BOOST_TEST(path1 == path2);
  }

  {
    FeaturePath const path1("c:/Program Files/source/layer/attribute",
         FeaturePath::WithAttribute);
    FeaturePath const path2("c:/Program Files/source/layer/attribute",
         FeaturePath::WithAttribute);
    BOOST_TEST(path1 == path2);
  }
}


BOOST_AUTO_TEST_CASE(assignment)
{
  using namespace dal;

  {
    FeaturePath const path1;
    const FeaturePath& path2 = path1;
    BOOST_TEST(path1 == path2);
  }

  {
    FeaturePath const path1("c:/Program Files/source/layer/attribute",
         FeaturePath::WithAttribute);
    const FeaturePath& path2 = path1;
    BOOST_TEST(path1 == path2);
  }
}


BOOST_AUTO_TEST_CASE(copy)
{
  using namespace dal;

  {
    FeaturePath const path1;
    const FeaturePath& path2(path1);
    BOOST_TEST(path1 == path2);
  }

  {
    FeaturePath const path1("c:/Program Files/source/layer/attribute",
         FeaturePath::WithAttribute);
    const FeaturePath& path2(path1);
    BOOST_TEST(path1 == path2);
  }
}
