#define BOOST_TEST_MODULE pcraster model_engine datatype
#include <boost/test/unit_test.hpp>
#include "calc_datatype.h"
#include "calc_datatypeclash.h"

BOOST_AUTO_TEST_CASE(testCtor)
{
  using namespace calc;

  {
    DataType const dt;
    BOOST_TEST(dt.vs() == VS_ANYTHING);
    BOOST_TEST(!dt.stNonSpatial());
    BOOST_TEST(!dt.stSpatial());
    BOOST_TEST(!dt.stEither());
    BOOST_TEST(dt.st() == ST_ALL);
  }
  {
    double const v = 4.56;
    DataType const dt(v);
    BOOST_TEST(dt.vs() == VS_SD);
    BOOST_TEST(dt.stNonSpatial());
    BOOST_TEST(!dt.stSpatial());
    BOOST_TEST(!dt.stEither());
  }
  {
    DataType const dt(VS_SD, ST_NONSPATIAL);
    BOOST_TEST(dt.vs() == VS_SD);
    BOOST_TEST(dt.stNonSpatial());
    BOOST_TEST(!dt.stSpatial());
    BOOST_TEST(!dt.stEither());
  }
  {
    DataType const dt(VS_SD, false);
    BOOST_TEST(dt.vs() == VS_SD);
    BOOST_TEST(dt.stNonSpatial());
    BOOST_TEST(!dt.stSpatial());
    BOOST_TEST(!dt.stEither());
  }
  {
    DataType const dt(VS_TSS);
    BOOST_TEST(dt.vs() == VS_TSS);
    BOOST_TEST(!dt.stNonSpatial());
    BOOST_TEST(!dt.stSpatial());
    BOOST_TEST(!dt.stEither());
    BOOST_TEST(dt.st() == ST_NON);
  }
}

BOOST_AUTO_TEST_CASE(testRestrict)
{
  using namespace calc;

  {
    DataType dt(VS_S, true);

    // VS
    bool catched = false;
    try {
      dt.restrict(VS_N);
    } catch (const VSClash &c) {
      BOOST_TEST(c.isOneOf() == VS_S);
      BOOST_TEST(c.mustBeOneOf() == VS_N);
      catched = true;
    }
    BOOST_TEST(catched);
    BOOST_TEST(dt.vs() == VS_S);

    // DataType
    catched = false;
    try {
      dt.restrict(DataType(VS_N, ST_DERIVED));
    } catch (const VSClash &c) {
      BOOST_TEST(c.isOneOf() == VS_S);
      BOOST_TEST(c.mustBeOneOf() == VS_N);
      catched = true;
    }
    BOOST_TEST(catched);
    BOOST_TEST(dt.vs() == VS_S);
  }
  {
    DataType dt(VS_TABLE);

    std::vector<VS> colTypes(2, VS_S);
    DataType r(VS_TABLE);
    r.setTableColTypes(colTypes);

    // dt has no colTypes
    BOOST_TEST(r != dt);
    BOOST_TEST(dt != r);
    BOOST_TEST(dt.tableColTypes().size() == 0);
    dt.restrict(r);
    // dt now has identical colTypes
    BOOST_TEST(dt.tableColTypes().size() == 2);
    BOOST_TEST(r == dt);
    BOOST_TEST(dt == r);
    dt.restrict(r);
    BOOST_TEST(r == dt);

    bool catched = false;
    colTypes.push_back(VS_N);
    DataType r1(VS_TABLE);
    r1.setTableColTypes(colTypes);
    BOOST_TEST(r1 != dt);  // test eq
    try {
      dt.restrict(r1);
    } catch (TableClash) {
      catched = true;
    }
    BOOST_TEST(catched);
  }
  {
    DataType r(VS_OBJECT);
    DataType dt(VS_OBJECT, ST_NON);
    BOOST_TEST(r == dt);
    dt.restrict(r);
    BOOST_TEST(r == dt);
    r.restrict(dt);
    BOOST_TEST(r == dt);
  }
}
