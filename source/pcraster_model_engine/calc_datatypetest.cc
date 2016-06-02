#define BOOST_TEST_MODULE pcraster model_engine datatype
#include <boost/test/unit_test.hpp>
#include "calc_datatype.h"
#include "calc_datatypeclash.h"


BOOST_AUTO_TEST_CASE(testCtor)
{
  using namespace calc;

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

BOOST_AUTO_TEST_CASE(testRestrict)
{
  using namespace calc;

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
