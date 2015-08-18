#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_DATATYPETEST
#include "calc_datatypetest.h"
#define INCLUDED_CALC_DATATYPETEST
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
#ifndef INCLUDED_CALC_DATATYPE
#include "calc_datatype.h"
#define INCLUDED_CALC_DATATYPE
#endif
#ifndef INCLUDED_CALC_DATATYPECLASH
#include "calc_datatypeclash.h"
#define INCLUDED_CALC_DATATYPECLASH
#endif


/*!
  \file
  This file contains the implementation of the DataTypeTest class.
*/

// NOTE use string failureExpected in files expected to fail, see style guide

//------------------------------------------------------------------------------
// DEFINITION OF STATIC DATATYPE MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*calc::DataTypeTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<DataTypeTest> instance(new DataTypeTest());

  suite->add(BOOST_CLASS_TEST_CASE(&DataTypeTest::testCtor, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&DataTypeTest::testRestrict, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF DATATYPE MEMBERS
//------------------------------------------------------------------------------

//! ctor
calc::DataTypeTest::DataTypeTest()
{
}



//! setUp
void calc::DataTypeTest::setUp()
{
}



//! tearDown
void calc::DataTypeTest::tearDown()
{
}



void calc::DataTypeTest::testCtor()
{
  {
   DataType dt;
   BOOST_CHECK(dt.vs()==VS_ANYTHING);
   BOOST_CHECK(!dt.stNonSpatial());
   BOOST_CHECK(!dt.stSpatial());
   BOOST_CHECK(!dt.stEither());
   BOOST_CHECK(dt.st()==ST_ALL);
  }
  {
   double v=4.56;
   DataType dt(v);
   BOOST_CHECK(dt.vs()==VS_SD);
   BOOST_CHECK( dt.stNonSpatial());
   BOOST_CHECK(!dt.stSpatial());
   BOOST_CHECK(!dt.stEither());
  }
  {
   DataType dt(VS_SD,ST_NONSPATIAL);
   BOOST_CHECK(dt.vs()==VS_SD);
   BOOST_CHECK( dt.stNonSpatial());
   BOOST_CHECK(!dt.stSpatial());
   BOOST_CHECK(!dt.stEither());
  }
  {
   DataType dt(VS_SD,false);
   BOOST_CHECK(dt.vs()==VS_SD);
   BOOST_CHECK( dt.stNonSpatial());
   BOOST_CHECK(!dt.stSpatial());
   BOOST_CHECK(!dt.stEither());
  }
  {
   DataType dt(VS_TSS);
   BOOST_CHECK(dt.vs()==VS_TSS);
   BOOST_CHECK(!dt.stNonSpatial());
   BOOST_CHECK(!dt.stSpatial());
   BOOST_CHECK(!dt.stEither());
   BOOST_CHECK(dt.st()==ST_NON);
  }
}

void calc::DataTypeTest::testRestrict()
{
  {
     DataType dt(VS_S,true);

     // VS
     bool catched=false;
     try {
      dt.restrict(VS_N);
     } catch(const VSClash& c) {
       BOOST_CHECK(c.isOneOf()==VS_S);
       BOOST_CHECK(c.mustBeOneOf() ==VS_N);
       catched=true;
     }
     BOOST_CHECK(catched);
     BOOST_CHECK(dt.vs()==VS_S);

     // DataType
     catched=false;
     try {
      dt.restrict(DataType(VS_N,ST_DERIVED));
     } catch(const VSClash& c) {
       BOOST_CHECK(c.isOneOf()==VS_S);
       BOOST_CHECK(c.mustBeOneOf() ==VS_N);
       catched=true;
     }
     BOOST_CHECK(catched);
     BOOST_CHECK(dt.vs()==VS_S);
  }
  {
     DataType dt(VS_TABLE);

     std::vector<VS> colTypes(2,VS_S);
     DataType r(VS_TABLE);
     r.setTableColTypes(colTypes);

     // dt has no colTypes
     BOOST_CHECK(r  != dt);
     BOOST_CHECK(dt != r);
     BOOST_CHECK(dt.tableColTypes().size()==0);
     dt.restrict(r);
     // dt now has identical colTypes
     BOOST_CHECK(dt.tableColTypes().size()==2);
     BOOST_CHECK(r  == dt);
     BOOST_CHECK(dt == r);
     dt.restrict(r);
     BOOST_CHECK(r  == dt);

     bool catched=false;
     colTypes.push_back(VS_N);
     DataType r1(VS_TABLE);
     r1.setTableColTypes(colTypes);
     BOOST_CHECK(r1 != dt); // test eq
     try {
      dt.restrict(r1);
     } catch (TableClash) {
       catched=true;
     }
     BOOST_CHECK(catched);
  }
  {
    DataType r(VS_OBJECT);
    DataType dt(VS_OBJECT,ST_NON);
    BOOST_CHECK(r  == dt);
    dt.restrict(r);
    BOOST_CHECK(r  == dt);
    r.restrict(dt);
    BOOST_CHECK(r  == dt);
  }
}
