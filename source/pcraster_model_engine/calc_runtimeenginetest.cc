#define BOOST_TEST_MODULE pcraster model_engine runtimeengine
#include <boost/test/unit_test.hpp>
#include "geo_rasterspace.h"
#include "com_exception.h"
#include "com_file.h"
#include "calc_datastorageid.h"
#include "calc_findsymbol.h"
#include "calc_rttypecheck.h"
#include "calc_spatial.h"
#include "calc_nonspatial.h"
#include "calc_operator.h"

#define private public
#include "calc_runtimeengine.h"


BOOST_AUTO_TEST_CASE(testPopField)
{
  using namespace calc;

 { // Test that different pointers are returned
   //  and both the in and out ptrs can be deleted
   //  correctly
  RunTimeEngine      rte(geo::RasterSpace(2,2));
  REAL4 zData[4]= { 1,1,1,1};
  Spatial *in=new Spatial(VS_S,zData,4);

  BOOST_CHECK(!in->readOnlyReference());
  BOOST_CHECK( in->pcrmeManaged());

  rte.pushField(in);

  Field *out= rte.releasePopField();
  BOOST_CHECK(in!=out);

  BOOST_CHECK(in->vs() == VS_S);
  BOOST_CHECK(in->src_f()[0] == 1);
  BOOST_CHECK(in->src_f()[3] == 1);

  BOOST_CHECK(out->vs() == VS_S);
  BOOST_CHECK(out->src_f()[0] == 1);
  BOOST_CHECK(out->src_f()[3] == 1);

  delete in;
  delete out;
 }
}


BOOST_AUTO_TEST_CASE(testCloneSet)
{
  using namespace calc;

  bool catched=false;
  try {
    // 0,0 is invalid
   RunTimeEngine      rte(geo::RasterSpace(0,0));
   const Operator* o = major2op(OP_NOT_);
   rtTypeCheck(*o,rte.d_rte,0);
  } catch(const com::Exception& e) {
BOOST_CHECK(e.messages().find("no clone or area map specified")
    != std::string::npos);
    catched=true;
  }
  BOOST_CHECK(catched);
}

BOOST_AUTO_TEST_CASE(testCloneDiffer)
{
  using namespace calc;

  bool catched=false;
  try {
    // 0,0 is invalid
   RunTimeEngine      rte(geo::RasterSpace(2,2));
   const Operator* o = major2op(OP_NOT_);
   UINT1 bData=0;
   rte.transferPushField(new Spatial(VS_B,&bData , 1)); // 2*2 != 1
   rtTypeCheck(*o,rte.d_rte,1);

  } catch(const com::Exception& e) {
BOOST_CHECK(e.messages().find(
         "Number of cells is different than clone or previous argument")
    != std::string::npos);
    catched=true;
  }
  BOOST_CHECK(catched);
}

BOOST_AUTO_TEST_CASE(testNrArgs)
{
  using namespace calc;

  bool catched=false;
  try {
    // 0,0 is invalid
   RunTimeEngine      rte(geo::RasterSpace(2,2));
   const Operator* o = major2op(OP_NOT_);
   rtTypeCheck(*o,rte.d_rte,0);

  } catch(const com::Exception& e) {
BOOST_CHECK(e.messages().find(
         "operator 'not' not enough arguments specified")
    != std::string::npos);
    catched=true;
  }
  BOOST_CHECK(catched);
}

BOOST_AUTO_TEST_CASE(testTypeCheck)
{
  using namespace calc;

 { // conversion
   RunTimeEngine      rte(geo::RasterSpace(2,2));
   const Operator* o = major2op(OP_SCALAR);
   REAL4 bData=1.0;
   NonSpatial* ns =new NonSpatial(VS_SD,bData);
   BOOST_CHECK(ns->vs()==VS_SD);
   rte.pushField(ns);

   rte.checkAndExec(*o,1);

   Field *f = rte.releasePopField();
   BOOST_CHECK(f->vs()==VS_S);
   delete f;
   delete ns;
 }
 {
   RunTimeEngine      rte(geo::RasterSpace(2,2));
   const Operator* o = major2op(OP_SPREAD);
   INT4 zData[4]= { 1,1,1,1};
   rte.transferPushField(new Spatial(VS_N,zData,4));
   rte.transferPushField(new NonSpatial(VS_FIELD,0));
   rte.transferPushField(new NonSpatial(VS_FIELD,1));

   rte.checkAndExec(*o,3);
   Field *f= rte.releasePopField();

   BOOST_CHECK(f->vs() == VS_S);
   delete f;
 }
 {
   RunTimeEngine      rte(geo::RasterSpace(2,2));
   const Operator* o = major2op(OP_SPREADZONE);
   INT4 zData[4]= { 1,1,1,1};
   rte.transferPushField(new Spatial(VS_N,zData,4));
   rte.transferPushField(new NonSpatial(VS_FIELD,0));
   rte.transferPushField(new NonSpatial(VS_FIELD,1));

   rte.checkAndExec(*o,3);
   Field *f= rte.releasePopField();

   BOOST_CHECK(f->vs() == VS_N);
   delete f;
 }
 {
   RunTimeEngine      rte(geo::RasterSpace(2,2));
   const Operator* o = major2op(OP_NE);
   UINT1 zData[4]= { 1,1,1,1};
   rte.transferPushField(new Spatial(VS_L,zData,4));
   rte.transferPushField(new NonSpatial(VS_FIELD,5));

   rte.checkAndExec(*o,2);
   Field *f= rte.releasePopField();

   BOOST_CHECK(f->vs() == VS_B);
   delete f;
 }
}

BOOST_AUTO_TEST_CASE(testResetVs)
{
  using namespace calc;

 {
   RunTimeEngine      rte(geo::RasterSpace(2,2));
   const Operator* o = major2op(OP_COVER);
   UINT1 zData[4]= { 1,1,1,1};
   rte.transferPushField(new Spatial(VS_L,zData,4));
   rte.transferPushField(new NonSpatial(VS_FIELD,5));

   rte.checkAndExec(*o,2);
   Field *f= rte.releasePopField();

   BOOST_CHECK(f->vs() == VS_L);
   delete f;
 }
 { // was a resetVS issue
   RunTimeEngine      rte(geo::RasterSpace(2,2));
   const Operator* o = major2op(OP_NOMINAL);
   INT4 zData[4]= { 1,1,1,1};

   rte.transferPushField(new Spatial(VS_O,zData,4));

   rte.checkAndExec(*o,1);
   Field *f= rte.releasePopField();

   BOOST_CHECK(f->vs() == VS_N);
   delete f;
 }
}

BOOST_AUTO_TEST_CASE(testBuildExpr)
{

   bool todoCoverMaxVarArgForPython=false;
   BOOST_WARN(todoCoverMaxVarArgForPython);

/* bug with building up the expression
 *  RunTimeEngine      rte(geo::RasterSpace(2,2));
 *  const Operator* o = major2op(OP_MAX);
 *  REAL4 zData[4]= { 1,1,1,1};
 *  rte.pushField(new Spatial(VS_S,zData,4));
 *  rte.pushField(new NonSpatial(VS_FIELD,5));
 *  rte.pushField(new NonSpatial(VS_FIELD,8));
 *
 *
 *  o->checkAndExec(&rte,3);
 *  Field *f= rte.releasePopField();
 *
 *  BOOST_CHECK(f->vs() == VS_S);
 *  BOOST_CHECK(f->src_f()[0] == 8);
 *  BOOST_CHECK(f->src_f()[3] == 8);
 *  delete f;
 */
}

BOOST_AUTO_TEST_CASE(testLoadByStorageId)
{
  using namespace calc;

  // test how data is stored in the rte->dataTable
  //  by passing it through a DataStorageId
  // was pcrcalc09
  // lookupnominal(inp_1.tbl, 5, 8, 10)

  // use a copy
  com::copy("inp_1.tbl","tmp.tbl");

#define NR6 6

  const Operator* a=major2op(OP_LOOKUPNOMINAL);

  NonSpatial o5(VS_O,(double)5);
  NonSpatial o8(VS_O,(double)8);
  NonSpatial o10(VS_O,(double)10);

  RunTimeEngine rte(geo::RasterSpace(1,NR6));

  {

    DataStorageId id("tmp.tbl");
    // pass reference pointer, no transfer of ownership
    rte.pushDataStorageId(&id);

    rte.pushField(&o5);
    rte.pushField(&o8);
    rte.pushField(&o10);


    rte.checkAndExec(*a,4); // 3 fields and the table
    { // nonspatial result original pcrcalc9 test
      Field *r=rte.releasePopField();

      BOOST_CHECK(!r->isSpatial());
      BOOST_CHECK( r->vs()==VS_N);
      BOOST_CHECK( r->src_4()[0]==1);
      delete r;
    }
  }
  // remove, to ensure next call will use already loaded version
  com::remove("tmp.tbl");
  {
    DataStorageId id("tmp.tbl");
    // pass reference pointer, no transfer of ownership
    rte.pushDataStorageId(&id);

    rte.pushField(&o5);
    INT4 vb[NR6] = { 8, 4, 4, 8, MV_INT4, MV_INT4 };
    rte.transferPushField(new Spatial(VS_O,vb,NR6));
    rte.pushField(&o10);

    rte.checkAndExec(*a,4); // 3 fields and the table
    {
      Field* r=rte.releasePopField();

      INT4 vbRes[NR6] = { 1, 0, 0, 1, MV_INT4, MV_INT4 };
      BOOST_CHECK(r->isSpatial());
      BOOST_CHECK(r->vs()==VS_N);
      BOOST_CHECK(std::equal(vbRes,vbRes+NR6,r->src_4()));
      delete r;
    }
  }
  // let's generated an error message
  { // one column short
    DataStorageId id("tmp.tbl");
    // pass reference pointer, no transfer of ownership
    rte.pushDataStorageId(&id);

    rte.pushField(&o5);
    rte.pushField(&o8);
    // one column short


    bool tableClash=false;
    try {
     rte.checkAndExec(*a,3); // 2 fields but 3+1 column  table
    } catch(const com::Exception& e) {
BOOST_CHECK(e.messages().find(
       "tmp.tbl: used as table with 4 columns, but has 3 columns")
        != std::string::npos);
      tableClash=true;
    }
    BOOST_CHECK(tableClash);

  }
  { // one column wrong
    DataStorageId id("tmp.tbl");
    // pass reference pointer, no transfer of ownership
    rte.pushDataStorageId(&id);

    rte.pushField(&o5);
    NonSpatial s8(VS_S,(double)8);
    rte.pushField(&s8); // one column wrong
    rte.pushField(&o8);


    bool tableClash=false;
    try {
     rte.checkAndExec(*a,4); // VS_S instead of VS_O
    } catch(const com::Exception& e) {
BOOST_CHECK(e.messages().find(
         "column '2' used as scalar type, but has ordinal type")
        != std::string::npos);
      tableClash=true;
    }
    BOOST_CHECK(tableClash);

  }
}
