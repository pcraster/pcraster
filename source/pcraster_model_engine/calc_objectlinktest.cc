#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_OBJECTLINKTEST
#include "calc_objectlinktest.h"
#define INCLUDED_CALC_OBJECTLINKTEST
#endif

// Library headers.
#ifndef INCLUDED_STDEXCEPT
#include <stdexcept>
#define INCLUDED_STDEXCEPT
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

// PCRaster library headers.
#ifndef INCLUDED_GEO_RASTERSPACE
#include "geo_rasterspace.h"
#define INCLUDED_GEO_RASTERSPACE
#endif
#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif

// Module headers.
#ifndef INCLUDED_CALC_FINDSYMBOL
#include "calc_findsymbol.h"
#define INCLUDED_CALC_FINDSYMBOL
#endif
#ifndef INCLUDED_CALC_OPERATIONS
#include "calc_operations.h"
#define INCLUDED_CALC_OPERATIONS
#endif
#ifndef INCLUDED_CALC_OPERATOR
#include "calc_operator.h"
#define INCLUDED_CALC_OPERATOR
#endif
#ifndef INCLUDED_CALC_RUNTIMEENV
#include "calc_runtimeenv.h"
#define INCLUDED_CALC_RUNTIMEENV
#endif
#ifndef INCLUDED_CALC_SPATIAL
#include "calc_spatial.h"
#define INCLUDED_CALC_SPATIAL
#endif
#ifndef INCLUDED_CALC_NONSPATIAL
#include "calc_nonspatial.h"
#define INCLUDED_CALC_NONSPATIAL
#endif


/*!
  \file
  This file contains the implementation of the ObjectLinkTest class.
*/

// NOTE use string failureExpected in files expected to fail, see style guide

//------------------------------------------------------------------------------
// DEFINITION OF STATIC OBJECTLINK MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*calc::ObjectLinkTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<ObjectLinkTest> instance(new ObjectLinkTest());

  suite->add(BOOST_CLASS_TEST_CASE(&ObjectLinkTest::testLoadLink, instance));
  // in this order, testLoadLink2 does the load
  suite->add(BOOST_CLASS_TEST_CASE(&ObjectLinkTest::testLoadLink2, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&ObjectLinkTest::testExec, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&ObjectLinkTest::testExec2, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&ObjectLinkTest::testNoArguments, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&ObjectLinkTest::testCheckAndExec, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF OBJECTLINK MEMBERS
//------------------------------------------------------------------------------

//! ctor
calc::ObjectLinkTest::ObjectLinkTest()
{
}



//! setUp
void calc::ObjectLinkTest::setUp()
{
}



//! tearDown
void calc::ObjectLinkTest::tearDown()
{
}

// getMeta()
#include "calcLibWrapper.cc"
//! to trace execution of ctor and methods
static size_t execCallMarker=0;

void calc::CalcLibDemoObjectLink::setDem(
    const REAL4         *dem)
{
  execCallMarker=100*(size_t)dem[0];
}
void calc::CalcLibDemoObjectLink::getDem(
    REAL4 *             dem)
{
  dem[2]=10;
}

void calc::CalcLibDemoObjectLink::noArguments()
{
}

void calc::CalcLibDemoObjectLink::testOrder(
    REAL4*                           result,
    const std::vector<const REAL4 *> dems,
    UINT1                            singleLdd)
{
  result[0]=dems[0][0];
  result[1]=dems[1][0];
  result[2]=singleLdd;
}

void calc::CalcLibDemoObjectLink::testOrder2(
    std::vector<REAL4*>&             result,
    const std::vector<const REAL4 *> dems,
    UINT1                            singleLdd) const
{
  PRECOND(result.size()==2);
  result[1][0]=dems[0][0];
  result[1][1]=dems[1][0];
  result[1][2]=singleLdd;
  for(size_t i=0; i < 3; ++i)
    result[0][i]=result[1][i]*10;
}

calc::CalcLibDemoObjectLink::CalcLibDemoObjectLink(
    const geo::RasterSpace& /* rs */)
{
  execCallMarker=99;
}

calc::CalcLibDemoObjectLink::~CalcLibDemoObjectLink()
{
  execCallMarker=77;
}

void calc::ObjectLinkTest::testLoadLink()
{
  ObjectLinkMeta olm=getMeta();

  BOOST_CHECK( olm.className()=="CalcLibDemoObjectLink");
  ObjectLinkMeta::MethodMap::const_iterator pos;
  // ctor
  pos = olm.methodMap().find("");
  BOOST_CHECK(pos != olm.methodMap().end());

  pos = olm.methodMap().find("getDem");
  BOOST_CHECK(pos != olm.methodMap().end());

  pos = olm.methodMap().find("failureExpected");
  BOOST_CHECK(pos == olm.methodMap().end());
}

void calc::ObjectLinkTest::testLoadLink2()
{
  // multiple loads harmless?
  globalOperations.load(getMeta);

  bool catched=false;
  try {
  globalOperations.load(getMeta);
  } catch(...) {
    catched=true;
  }
  BOOST_CHECK(catched);


  // ctor has className
  const Operator *o;
  o=globalOperations["CalcLibDemoObjectLink"];
  BOOST_CHECK(o);
  BOOST_CHECK(o->nrResults()       == 1);
  BOOST_CHECK(o->resultType(0).vs()== VS_OBJECT);

  // method
  o=globalOperations["CalcLibDemoObjectLink::getDem"];
  BOOST_CHECK(o);
  BOOST_CHECK(o->nrResults()       == 1);
  BOOST_CHECK(o->resultType(0).vs()== VS_S);

  //! unknown method
  BOOST_CHECK(globalOperations["CalcLibDemoObjectLink::failureExpected"] == 0);
}

void calc::ObjectLinkTest::testExec()
{
 RunTimeEnv      rte(geo::RasterSpace(1,3));
 // test only the calls discarding RunTimeEnv info
 BOOST_CHECK(execCallMarker==0);

 ObjectLinkProxy<CalcLibDemoObjectLink>* t= 
       new ObjectLinkProxy<CalcLibDemoObjectLink>("",rte.rasterSpace(),0);

 {
   std::vector<Field *> f;
   REAL4 demIn[3]= { 4,8,16};
   f.push_back(new Spatial(VS_S,demIn,3));
   t->exec1("setDem",f);
   BOOST_CHECK(execCallMarker==400);
   delete f[0];
 }
 {
   std::vector<Field *> f;
   f.push_back(new Spatial(VS_S,CRI_f,3));
   f[0]->dest_f()[2] = -1;
   t->exec1("getDem",f);
   BOOST_CHECK(f[0]->src_f()[2] == 10);
   delete f[0];
 }
 {
   // test order of arguments
   std::vector<Field *> f;
   f.push_back(new Spatial(VS_S,CRI_f,3)); // result

   REAL4 demIn[3]= { 10,16,16};
   f.push_back(new Spatial(VS_S,demIn,3)); // dems 0
   demIn[0]= 20;
   f.push_back(new Spatial(VS_S,demIn,3)); // dems 1

   UINT1 l(5);
   f.push_back(new NonSpatial(VS_L,l));    // bool arg
   BOOST_CHECK(f.back()->cri() == CRI_1);
   t->exec1("testOrder",f);

   BOOST_CHECK(f[0]->src_f()[0] == 10);
   BOOST_CHECK(f[0]->src_f()[1] == 20);
   BOOST_CHECK(f[0]->src_f()[2] == 5);
   for (size_t i=0; i < f.size(); ++i)
     delete f[i];
 }

 bool catched=false;
 // unknown method
 try {
  std::vector<Field *> f;
  t->exec1("failureExpected",f);
 } catch(const ObjectLink::UnknownMethod& ) {
   catched=true;
 }
 BOOST_CHECK(catched);

 catched=false;
 // not enough arguments
 try {
  std::vector<Field *> f;
  t->exec1("testOrder",f);
 } catch(const std::out_of_range&) {
   catched=true;
 }
 BOOST_CHECK(catched);

 delete t;
 BOOST_CHECK(execCallMarker==77);
}

void calc::ObjectLinkTest::testExec2()
{
  {
   RunTimeEnv      rte(geo::RasterSpace(2,2));

   // ctor
   const Operator *o;
   o=opName2op("CalcLibDemoObjectLink");
   BOOST_CHECK(o);
   o->exec(&rte,0);
   BOOST_CHECK(execCallMarker==99);

   // pop created ObjectLink
   DataValue *dv = rte.popDataValue();

   // now no ObjectLink present
   bool catched(false);
   try {
    o=opName2op("CalcLibDemoObjectLink::getDem");
    BOOST_CHECK(o);
    o->exec(&rte,0);
   } catch(const com::Exception& e) {
BOOST_CHECK(e.messages().find( "no ObjectLink present") != std::string::npos);
     catched=true;
   }
   BOOST_CHECK(catched);

   // delete ObjectLink
   delete dv;
   BOOST_CHECK(execCallMarker==77);
  }
}



void calc::ObjectLinkTest::testNoArguments()
{
  RunTimeEnv rte(geo::RasterSpace(2,2));
  Operator const* op = opName2op("CalcLibDemoObjectLink");
  BOOST_CHECK(op);
  op->exec(&rte, 0);
  op = opName2op("CalcLibDemoObjectLink::noArguments");
  BOOST_CHECK(op);
  DataValue *dv = rte.popDataValue();
  BOOST_CHECK(dv);
  delete dv;
}



void calc::ObjectLinkTest::testCheckAndExec()
{

 RunTimeEnv      rte(geo::RasterSpace(1,3));

 // ctor
 const Operator *o;
 o=globalOperations["CalcLibDemoObjectLink"];
 BOOST_CHECK(o);

 // wrong ctor call
 bool catched(false);
 try {
   // use createResultField to create the (wrong) input argument
  rte.pushField(rte.createResultField(DataType(VS_S,ST_NON)));
  o->checkAndExec(&rte,1);
 } catch (const com::Exception& e) {
BOOST_CHECK(e.messages().find(
         "'CalcLibDemoObjectLink' too many arguments specified") != std::string::npos);
   catched=true;
 }
 BOOST_CHECK(catched);

 // good ctor
 o->checkAndExec(&rte,0);

 // pop created ObjectLink
 DataValue *dv = rte.popDataValue();
 BOOST_CHECK(dynamic_cast<ObjectLink*>(dv));

 {
   REAL4 demIn[3]= { 10,16,16};
   rte.pushDataValue(dv);
   rte.pushField(new Spatial(VS_S,demIn,3));
   o=opName2op("CalcLibDemoObjectLink::setDem");
   BOOST_CHECK(o);
   BOOST_CHECK(o->nrResults()==0);

   o->checkAndExec(&rte,1);
 }

 // exec something
 {
   rte.pushDataValue(dv);

   REAL4 demIn[3]= { 10,16,16};
   rte.pushField(new Spatial(VS_S,demIn,3)); // dems 0
   demIn[0]= 20;
   rte.pushField(new Spatial(VS_S,demIn,3)); // dems 1

   UINT1 l(5);
   rte.pushField(new NonSpatial(VS_L,l));    // bool arg

   o=globalOperations["CalcLibDemoObjectLink::testOrder"];
   BOOST_CHECK(o);
   o->checkAndExec(&rte,3);

   Field *res= rte.popField();

   BOOST_CHECK(res->isSpatial());
   BOOST_CHECK(res->cri()==CRI_f);
   BOOST_CHECK(res->src_f()[0] == 10);
   BOOST_CHECK(res->src_f()[1] == 20);
   BOOST_CHECK(res->src_f()[2] == 5);
   delete res;
 }

 {
   rte.pushDataValue(dv);

   REAL4 demIn[3]= { 10,16,16};
   rte.pushField(new Spatial(VS_S,demIn,3)); // dems 0
   demIn[0]= 20;
   rte.pushField(new Spatial(VS_S,demIn,3)); // dems 1

   UINT1 l(5);
   rte.pushField(new NonSpatial(VS_L,l));    // bool arg

   o=globalOperations["CalcLibDemoObjectLink::testOrder2"];
   BOOST_CHECK(o);
   o->checkAndExec(&rte,3);

   // reversed stack order
   Field *res1= rte.popField();
   Field *res0= rte.popField();

   BOOST_CHECK(res1->isSpatial());
   BOOST_CHECK(res1->cri()==CRI_f);
   BOOST_CHECK(res1->src_f()[0] == 10);
   BOOST_CHECK(res1->src_f()[1] == 20);
   BOOST_CHECK(res1->src_f()[2] == 5);
   BOOST_CHECK(res0->isSpatial());
   BOOST_CHECK(res0->cri()==CRI_f);
   BOOST_CHECK(res0->src_f()[0] == 100);
   BOOST_CHECK(res0->src_f()[1] == 200);
   BOOST_CHECK(res0->src_f()[2] == 50);
   delete res0;
   delete res1;
 }


 // now no ObjectLink present
 catched=false;
 try {
  o=globalOperations["CalcLibDemoObjectLink::getDem"];
  BOOST_CHECK(o);
  o->exec(&rte,0);
 } catch(const com::Exception& e) {
BOOST_CHECK(e.messages().find( "no ObjectLink present") != std::string::npos);
   catched=true;
 }
 BOOST_CHECK(catched);

 // delete ObjectLink
 delete dv;
 BOOST_CHECK(execCallMarker==77);
}
