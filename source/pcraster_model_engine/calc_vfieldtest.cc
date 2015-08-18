#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_VFIELDTEST
#include "calc_vfieldtest.h"
#define INCLUDED_CALC_VFIELDTEST
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
#ifndef INCLUDED_CALC_VFIELD
#include "calc_vfield.h"
#define INCLUDED_CALC_VFIELD
#endif
#ifndef INCLUDED_CALC_NONSPATIAL
#include "calc_nonspatial.h"
#define INCLUDED_CALC_NONSPATIAL
#endif
#ifndef INCLUDED_CALC_SPATIAL
#include "calc_spatial.h"
#define INCLUDED_CALC_SPATIAL
#endif

/*!
  \file
  This file contains the implementation of the VFieldTest class.
*/

// NOTE use string failureExpected in files expected to fail, see style guide

//------------------------------------------------------------------------------
// DEFINITION OF STATIC VFIELD MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*calc::VFieldTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<VFieldTest> instance(new VFieldTest());

  suite->add(BOOST_CLASS_TEST_CASE(&VFieldTest::test, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&VFieldTest::testUpdateMV, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF VFIELD MEMBERS
//------------------------------------------------------------------------------

//! ctor
calc::VFieldTest::VFieldTest()
{
}



//! setUp
void calc::VFieldTest::setUp()
{
}



//! tearDown
void calc::VFieldTest::tearDown()
{
}



void calc::VFieldTest::test()
{
  {
   int ns=3;
   VField<int> vf(ns,5);
   BOOST_CHECK(!vf.spatial());
   BOOST_CHECK(vf.size()==5);
   BOOST_CHECK(vf[0]==3);
   BOOST_CHECK(vf[2]==3);
   BOOST_CHECK(vf[4]==3);
  }
  {
   int s[5]={10,11,12,13,14};
   VField<int> vf(s,5);
   BOOST_CHECK(vf.spatial());
   BOOST_CHECK(vf.size()==5);
   BOOST_CHECK(vf[0]==10);
   BOOST_CHECK(vf[2]==12);
   BOOST_CHECK(vf[4]==14);
  }
  {
   Field *f(new NonSpatial(VS_N,3));
   VField<INT4> vf(f,5);
   BOOST_CHECK(!vf.spatial());
   BOOST_CHECK(vf.size()==5);
   BOOST_CHECK(vf[0]==3);
   BOOST_CHECK(vf[2]==3);
   BOOST_CHECK(vf[4]==3);
  }
  {
   INT4 data[5]={10,11,12,13,14};
   Field *f(new Spatial(VS_N,data,5));

   VField<INT4> vf(f,5);
   BOOST_CHECK(vf.spatial());
   BOOST_CHECK(vf.size()==5);
   BOOST_CHECK(vf[0]==10);
   BOOST_CHECK(vf[2]==12);
   BOOST_CHECK(vf[4]==14);
  }
}

void calc::VFieldTest::testUpdateMV()
{
  BitField bf(5);
  BOOST_CHECK(bf.none());

  {
   INT4 s[5]={10,11,MV_INT4,13,14};
   VField<INT4> vf(s,5);
   vf.updateMVField(bf);
   BOOST_CHECK(bf.count()==1);
   BOOST_CHECK(bf[2]==1);

  }

  {
   UINT1 s[5]={10,11,13,14,MV_UINT1};
   VField<UINT1> vf(s,5);
   vf.updateMVField(bf);
   BOOST_CHECK(bf.count()==2);
   BOOST_CHECK(bf[2]==1);
   BOOST_CHECK(bf[4]==1);
  }

  {
   Field *f(new NonSpatial(VS_N,3));
   VField<INT4> vf(f,5);
   BOOST_CHECK(bf.count()==2);
   BOOST_CHECK(bf[2]==1);
   BOOST_CHECK(bf[4]==1);
  }
}
