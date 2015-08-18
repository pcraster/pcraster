#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_COM_CLONETEST
#include "com_clonetest.h"
#define INCLUDED_COM_CLONETEST
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
#ifndef INCLUDED_COM_INTERVALTYPES
#include "com_intervaltypes.h"
#define INCLUDED_COM_INTERVALTYPES
#endif
#ifndef INCLUDED_COM_CLONE
#include "com_clone.h"
#define INCLUDED_COM_CLONE
#endif


/*!
  \file
  This file contains the implementation of the CloneTest class.
*/

// NOTE use string failureExpected in files expected to fail, see style guide

//------------------------------------------------------------------------------
// DEFINITION OF STATIC CLONE MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*com::CloneTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<CloneTest> instance(new CloneTest());

  suite->add(BOOST_CLASS_TEST_CASE(&CloneTest::testResetClone, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF CLONE MEMBERS
//------------------------------------------------------------------------------

//! ctor
com::CloneTest::CloneTest()
{
}



//! setUp
void com::CloneTest::setUp()
{
}

//! tearDown
void com::CloneTest::tearDown()
{
}



void com::CloneTest::testResetClone()
{
  typedef Interval<float> I; // virtual base of I
  typedef EqualTo<float>  E;
  {
    E* dest=new E(4);
    E* src(0);
    resetClone(dest,src);
    BOOST_CHECK(dest==0);

    dest=new E(8);
    BOOST_CHECK(dest->min()==8);
    resetClone(dest,new E(2));
    BOOST_CHECK(dest->min()==2);

    delete dest;
  }
}
