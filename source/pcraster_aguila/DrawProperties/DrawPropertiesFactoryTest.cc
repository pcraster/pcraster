#ifndef INCLUDED_DRAWPROPERTIESFACTORYTEST
#include "DrawPropertiesFactoryTest.h"
#define INCLUDED_DRAWPROPERTIESFACTORYTEST
#endif

// External headers.
#ifndef INCLUDED_IOSTREAM
#include <iostream>
#define INCLUDED_IOSTREAM
#endif

#ifndef INCLUDED_TYPEINFO
#include <typeinfo>
#define INCLUDED_TYPEINFO
#endif

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

#ifndef INCLUDED_LOKI_FACTORY
#include "loki/Factory.h"
#define INCLUDED_LOKI_FACTORY
#endif

// Project headers.

// Module headers.



/*!
  \file
  This file contains the implementation of the DrawPropertiesFactoryTest class.
*/

namespace {

class DrawPropertiesBase
{
public:
  virtual ~DrawPropertiesBase() {}
};

class DrawProperties1: public DrawPropertiesBase
{
};

DrawPropertiesBase* createDrawProperties1()
{
  return new DrawProperties1;
}

int const properties1 = 1;

class DrawProperties2: public DrawPropertiesBase
{
};

DrawPropertiesBase* createDrawProperties2()
{
  return new DrawProperties2;
}

int const properties2 = 2;

} // Anonymous namespace



namespace ag {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC DRAWPROPERTIESFACTORYTEST MEMBERS
//------------------------------------------------------------------------------

//! Suite.
boost::unit_test::test_suite* DrawPropertiesFactoryTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<DrawPropertiesFactoryTest> instance(
         new DrawPropertiesFactoryTest());
  suite->add(BOOST_CLASS_TEST_CASE(
         &DrawPropertiesFactoryTest::test, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF DRAWPROPERTIESFACTORYTEST MEMBERS
//------------------------------------------------------------------------------

//! Constructor.
DrawPropertiesFactoryTest::DrawPropertiesFactoryTest()
{
}



void DrawPropertiesFactoryTest::test()
{
  // Create a factory.
  typedef Loki::Factory<DrawPropertiesBase, int, Loki::NullType>
         DrawPropertiesFactory;
  DrawPropertiesFactory factory;

  // Register creation methods.
  factory.Register(properties1, createDrawProperties1);
  factory.Register(properties2, createDrawProperties2);

  // Let the factory create objects.
  {
    boost::shared_ptr<DrawPropertiesBase> object(
         factory.CreateObject(properties1));
    BOOST_REQUIRE(object);
    BOOST_CHECK(dynamic_cast<DrawProperties1*>(object.get()));
  }

  {
    boost::shared_ptr<DrawPropertiesBase> object(
         factory.CreateObject(properties2));
    BOOST_REQUIRE(object);
    BOOST_CHECK(dynamic_cast<DrawProperties2*>(object.get()));
  }
}

} // namespace ag

