#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_BUILDTYPESVISITORTEST
#include "calc_buildtypesvisitortest.h"
#define INCLUDED_CALC_BUILDTYPESVISITORTEST
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
#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif

// Module headers.
#ifndef INCLUDED_CALC_BUILDTYPESVISITOR
#include "calc_buildtypesvisitor.h"
#define INCLUDED_CALC_BUILDTYPESVISITOR
#endif
#ifndef INCLUDED_CALC_ASTCFGTESTER
#include "calc_astcfgtester.h"
#define INCLUDED_CALC_ASTCFGTESTER
#endif
#ifndef INCLUDED_CALC_ASTPAR
#include "calc_astpar.h"
#define INCLUDED_CALC_ASTPAR
#endif
#ifndef INCLUDED_CALC_ASTNUMBER
#include "calc_astnumber.h"
#define INCLUDED_CALC_ASTNUMBER
#endif
#ifndef INCLUDED_CALC_POSITIONNAME
#include "calc_positionname.h"
#define INCLUDED_CALC_POSITIONNAME
#endif
#ifndef INCLUDED_CALC_ASTEXPR
#include "calc_astexpr.h"
#define INCLUDED_CALC_ASTEXPR
#endif
#ifndef INCLUDED_CALC_STRINGPARSER
#include "calc_stringparser.h"
#define INCLUDED_CALC_STRINGPARSER
#endif
#ifndef INCLUDED_CALC_ASTPATH
#include "calc_astpath.h"
#define INCLUDED_CALC_ASTPATH
#endif
#ifndef INCLUDED_CALC_DATATYPECLASH
#include "calc_datatypeclash.h"
#define INCLUDED_CALC_DATATYPECLASH
#endif
/*!
  \file
  This file contains the implementation of the BuildTypesVisitorTest class.
*/

//------------------------------------------------------------------------------
// DEFINITION OF STATIC BUILDTYPESVISITOR MEMBERS
//------------------------------------------------------------------------------

#define EXPECT_ERROR(v,n,msgId)  \
{                                \
    TRY_TEST_MSG {               \
      v.visit();                 \
    } CATCH_TEST_MSG(msgId);     \
}

#define DEFAULT_BTV \
    BuildTypesVisitor btv(n.cfg()); \
    PRECOND(d_inputTable);          \
    btv.init(*d_inputTable);

#define N_BTV_VISIT(code)          \
    ASTCFGTester n(code);          \
    BuildTypesVisitor btv(n.cfg());\
    btv.visit();

//! suite
boost::unit_test::test_suite*calc::BuildTypesVisitorTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<BuildTypesVisitorTest> instance(new BuildTypesVisitorTest());

  suite->add(BOOST_CLASS_TEST_CASE(&BuildTypesVisitorTest::testPar, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&BuildTypesVisitorTest::testNumber, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&BuildTypesVisitorTest::testExpr, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&BuildTypesVisitorTest::testModel, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&BuildTypesVisitorTest::testReportParsed, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&BuildTypesVisitorTest::testErrorExpr, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&BuildTypesVisitorTest::testArgCombError, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&BuildTypesVisitorTest::testAssError, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&BuildTypesVisitorTest::testNonFieldError, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&BuildTypesVisitorTest::testMultipleVisits, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&BuildTypesVisitorTest::testNumberTyping, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&BuildTypesVisitorTest::testDoubleFuncRelic, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&BuildTypesVisitorTest::testRepeat, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&BuildTypesVisitorTest::testTopDownExprRestrictor, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF BUILDTYPESVISITOR MEMBERS
//------------------------------------------------------------------------------

//! ctor
calc::BuildTypesVisitorTest::BuildTypesVisitorTest():
 d_inputTable(0)
{
 setUp();
}

calc::BuildTypesVisitorTest::~BuildTypesVisitorTest()
{
 tearDown();
}

void calc::BuildTypesVisitorTest::insertTestTable(
    const std::string& name,
    const DataType& ft)
{
  ASTPar *p=createPar(name);
  d_tt_pars.push_back(p); // for deletion
  PositionName pn("testTable");
  p->setPosition(&pn);
  (*d_inputTable)[p].dataType()=ft;
  // act as if name is created somewhere first
  (*d_inputTable)[p].setIoType(IOType(
           pcrxml::ModelInputType::None,pcrxml::ModelOutputType::Fixed));
}


//! setUp
void calc::BuildTypesVisitorTest::setUp()
{
  d_inputTable = new ASTSymbolTable();

  insertTestTable("inp1s.map" ,DataType(VS_S,ST_SPATIAL ));
  insertTestTable("inp90d.map" ,DataType(VS_D,ST_SPATIAL ));
  insertTestTable("inp5s.map" ,DataType(VS_S,ST_SPATIAL ));
  insertTestTable("inp1b.map" ,DataType(VS_B,ST_SPATIAL ));
  insertTestTable("inp1n.map" ,DataType(VS_N,ST_SPATIAL ));
  insertTestTable("inpxo.map" ,DataType(VS_O,ST_SPATIAL ));

}

//! tearDown
void calc::BuildTypesVisitorTest::tearDown()
{
  delete d_inputTable;
  d_inputTable=0;
  for(size_t i=0; i< d_tt_pars.size(); ++i)
    delete d_tt_pars[i];
  d_tt_pars.clear();
}


void calc::BuildTypesVisitorTest::testPar()
{

 { // get type
   ASTPar *p= createPar("inp1s.map");
   ASTCFGTester n(p); // n will delete p

   ASTSymbolTable inputTable;
   inputTable[p].dataType()=DataType(VS_S,ST_SPATIAL);

   BuildTypesVisitor btv(n.cfg());
   btv.init(inputTable);
   btv.visit();

   DataType  btvRes(n.ast()->returnDataType());
   BOOST_CHECK(btvRes.vs() == VS_S);
   BOOST_CHECK(btvRes.stSpatial());
 }

 { // restrict type
   ASTPar *p= createPar("inp1s.map");
   ASTCFGTester n(p); // n will delete p

   ASTSymbolTable inputTable;
   inputTable[p].dataType()=DataType(VS_S,ST_SPATIAL);
   BuildTypesVisitor btv(n.cfg());
   btv.init(inputTable);

   bool catched=false;
   try {
    btv.visit();
    n.ast()->returnDataType().restrict(DataType(VS_N,ST_DERIVED));
   } catch(const VSClash& c) {
     BOOST_CHECK(c.isOneOf() == VS_S);
     BOOST_CHECK(c.mustBeOneOf() == VS_N);
     catched=true;
   }
   BOOST_CHECK(catched);
 }

 { // par can be anything
   ASTCFGTester n(createPar("anything"));
   BuildTypesVisitor btv(n.cfg());

   btv.visit();
   DataType btvRes(n.ast()->returnDataType());
   BOOST_CHECK(btvRes.vs() == VS_ANYTHING);
 }

}

void calc::BuildTypesVisitorTest::testNumber()
{
  ASTCFGTester n(createNumber("0.5"));
  BuildTypesVisitor btv(n.cfg());

 {
   btv.visit();
   DataType btvRes(n.ast()->returnDataType());
   BOOST_CHECK( btvRes.vs() == VS_SD);
   BOOST_CHECK( btvRes.stNonSpatial());
 }

 {
   bool catched=false;
   try {
    btv.visit();
    n.ast()->returnDataType().restrict(DataType(VS_N,ST_DERIVED));
   } catch(const VSClash& c) {
     BOOST_CHECK(c.isOneOf() == VS_SD);
     BOOST_CHECK(c.mustBeOneOf() == VS_N);
     catched=true;
   }
   BOOST_CHECK(catched);
 }
}

void calc::BuildTypesVisitorTest::testModel()
{
  // misc tests on a whole script
  {
   ASTCFGTester n(createFromId("pcrcalc376"));
   BuildTypesVisitor btv(n.cfg());
   // no visit no dynamic section found
   BOOST_CHECK(!btv.containsDynamicSection());
  }
  { // this is not the error message test for 376
    ASTCFGTester n(createFromId("pcrcalc376"));
    BuildTypesVisitor btv(n.cfg());
   // visit, dynamic section found
    btv.visit();
    BOOST_CHECK(btv.containsDynamicSection());
  }
  {
    ASTCFGTester n(createFromId("pcrcalc60"));
    BuildTypesVisitor btv(n.cfg());
    btv.visit();
    BOOST_CHECK(!btv.containsDynamicSection());
  }
  {
    ASTCFGTester n(createFromId("pcrcalc505"));
    BuildTypesVisitor btv(n.cfg());

    for(size_t i=0; i < 4; ++i) {
      btv.visit();
      // std::cout << " i(" << i << "), nrChanges(" << btv.nrChanges() << ")\n"
      //        << btv.table() << std::endl;
    }
  }
  { // test setFirstAss
    ASTCFGTester n(StringParser::createCodeAsNode(
           "a=1;a=2"));
    BuildTypesVisitor btv(n.cfg());
    btv.visit();

    ASTPar *l0= astCast<ASTPar>(n.ast(),"C/b/0/a/<"),
           *l1= astCast<ASTPar>(n.ast(),"C/b/1/a/<");
    BOOST_CHECK(l0->name()=="a");
    BOOST_CHECK(l1->name()=="a");
    BOOST_CHECK(btv.table()["a"].d_firstAss == l0);
  }
  { // test setFirstAss
    N_BTV_VISIT(StringParser::createCodeAsNode("p=a;a=2"));

    ASTPar *l0= astCast<ASTPar>(n.ast(),"C/b/0/a/>/p"),
           *l1= astCast<ASTPar>(n.ast(),"C/b/1/a/<");
    BOOST_CHECK(l0->name()=="a");
    BOOST_CHECK(l1->name()=="a");
    BOOST_CHECK(btv.table()["a"].d_firstAss          == l1);
  }
  {
    ASTCFGTester n(createFromId("pcrcalc532"));
    BuildTypesVisitor btv(n.cfg());
    btv.init(*d_inputTable);
    // should not throw due to hack in
    //  calc::Operator::isPolyInput(size_t i) const
    btv.visit();
  }

  {
    N_BTV_VISIT(StringParser::createCodeAsNode(
           "tmp.res=cover(cover.Expr1.imap,sqrt(9));"));
    // visit i=0 in macro
    for(size_t i=1; i < 9; ++i) {
      btv.visit();
    }
      bool todoAddTESTPATHandAresolveUNtil=false;
      BOOST_WARN(todoAddTESTPATHandAresolveUNtil);
  }
}


void calc::BuildTypesVisitorTest::testExpr()
{
  {
    ASTCFGTester n(createCode("<e v='+'>       \
                                <n v='3'/>    \
                                <p v='inp1s.map'/>    \
                           </e>"));
    DEFAULT_BTV;
    btv.visit();
    DataType btvRes(n.ast()->returnDataType());
    BOOST_CHECK(btvRes.vs() == VS_S);
    BOOST_CHECK(btvRes.stSpatial());
  }
  { // no arguments
    ASTCFGTester n(createCode("<e v='mapuniform'> \
                           </e>"));

    DEFAULT_BTV;
    btv.visit();
    DataType btvRes(n.ast()->returnDataType());
    BOOST_CHECK(btvRes.vs() == VS_S);
    BOOST_CHECK(btvRes.stNonSpatial());
  }
  { // poly arguments
    // st can be both
    ASTCFGTester n(createCode("<e v='mapmaximum'> \
                                <p v='inp1s.map'/>    \
                           </e>"));

    DEFAULT_BTV;
    btv.visit();
    DataType btvRes(n.ast()->returnDataType());
    BOOST_CHECK(btvRes.vs() == VS_S);
    BOOST_CHECK(btvRes.stNonSpatial());

    // force st to nonspatial
    btv.visit();
    {
    DataType btvRes(n.ast()->returnDataType());
    BOOST_CHECK(btvRes.vs() == VS_S);
    BOOST_CHECK(btvRes.stNonSpatial());
    }

    bool catched(false);
    try {
    btv.visit();
    // force st to spatial but it is not
    n.ast()->returnDataType().restrict(DataType(VS_FIELD,ST_SPATIAL));
    } catch (const STClash& s) {
      BOOST_CHECK(!s.spatialFound());
      catched=true;
    }
    BOOST_CHECK(catched);

  }
  {
    ASTCFGTester n(createCode("<e v='spreadzone' p='8'>   \
                             <e v='or'><n v='0'/><p v='inp1b.map'/></e> \
                             <n v='2'/>       \
                             <n v='3'/>       \
                           </e>"));
    DEFAULT_BTV;
    btv.visit();
    DataType btvRes(n.ast()->returnDataType());
    BOOST_CHECK(btvRes.vs() == VS_B);
    BOOST_CHECK(btvRes.stSpatial());
  }
  {
    ASTCFGTester n(createCode("<e v='nodirection'>       \
                                <p v='inp90d.map'/>    \
                               </e>"));
    DEFAULT_BTV;
    btv.visit();
    DataType btvRes(n.ast()->returnDataType());
    BOOST_CHECK(btvRes.vs() == VS_B);
    BOOST_CHECK(btvRes.stSpatial());
  }
  {
    ASTCFGTester n(createCode("<e v='nodirection'>       \
                                <n v='1'/>    \
                               </e>"));
    DEFAULT_BTV;
    btv.visit();
    DataType btvRes(n.ast()->returnDataType());
    BOOST_CHECK(btvRes.vs() == VS_B);
    BOOST_CHECK(btvRes.stNonSpatial());
  }
  {
    const char s[] ="succ(4) eq if(e , a , if ( e2 , b , c))";
    ASTCFGTester n(StringParser::createExpr(s));
    DEFAULT_BTV;
    btv.visit();
    DataType btvRes(n.ast()->returnDataType());
    BOOST_CHECK(btvRes.vs() == VS_B);
    BOOST_CHECK(btvRes.stEither());
  }
}

void calc::BuildTypesVisitorTest::testReportParsed()
{
  {
    const char s[]="foo = succ(4); bar = if(e , a , if ( e2 , b , c));";
    N_BTV_VISIT(StringParser::createCodeAsNode(s));
    BOOST_CHECK(!btv.hasStatementWithReportKeyword());
  }
  {
    const char s[]="foo = succ(4); report bar = if(e , a , if ( e2 , b , c));";
    N_BTV_VISIT(StringParser::createCodeAsNode(s));
    BOOST_CHECK(btv.hasStatementWithReportKeyword());
  }
}

void calc::BuildTypesVisitorTest::testErrorExpr()
{

  {
    ASTCFGTester n(createCode("<e v='spread' p='8'>   \
                                <n v='1'/>        \
                                <n v='1'/>        \
                                <n v='1'/>        \
                           </e>"));
    DEFAULT_BTV;
    EXPECT_ERROR(btv,n,"pcrcalc258");
  }
  {
    ASTCFGTester n(createCode("<e v='spread' p='8'>   \
                                <n p='9' v='1'/>  \
                                <n v='1'/>        \
                           </e>"));
    DEFAULT_BTV;
    EXPECT_ERROR(btv,n,"pcrcalc252");
  }
  {
    ASTCFGTester n(createCode("<e v='maparea' p='8'/>"));
    DEFAULT_BTV;
    EXPECT_ERROR(btv,n,"pcrcalc252a");
  }
  {
    ASTCFGTester n(createCode("<e v='spread' p='8'>   \
                                <n p='9' v='1'/>  \
                                <n v='1'/>        \
                                <n v='1'/>        \
                                <n v='1'/>        \
                           </e>"));
    DEFAULT_BTV;
    EXPECT_ERROR(btv,n,"pcrcalc253");
  }
  {
    ASTCFGTester n(createCode("<e v='spread' p='8'>   \
                             <e v='+'><n v='1'/><n v='1'/></e> \
                             <n v='1'/>       \
                             <n v='1'/>       \
                           </e>"));
    DEFAULT_BTV;
    EXPECT_ERROR(btv,n,"pcrcalc500");
  }
  { // modellink
    ASTCFGTester n(createFromId("pcrcalc548"));
    DEFAULT_BTV;
    EXPECT_ERROR(btv,n,"pcrcalc548");
  }
  { // unknown function
    ASTCFGTester n(createFromId("pcrcalc549"));
    DEFAULT_BTV;
    EXPECT_ERROR(btv,n,"pcrcalc549");
  }
  { // unknown method
    ASTCFGTester n(createFromId("pcrcalc550"));
    DEFAULT_BTV;
    EXPECT_ERROR(btv,n,"pcrcalc550");
  }
  if (0) { // nog niet noodzakelijk
    ASTCFGTester n(createFromId("pcrcalc102a"));
    DEFAULT_BTV;
    EXPECT_ERROR(btv,n,"pcrcalc102a");
  }
}

void calc::BuildTypesVisitorTest::testArgCombError()
{
  {
    ASTCFGTester n(createCode("<e v='cover'>   \
                                <p v='inp5s.map'/>        \
                                <p v='inp5s.map'/>        \
                                <p p='8' v='inp1b.map'/>  \
                           </e>"));
    DEFAULT_BTV;
    EXPECT_ERROR(btv,n,"pcrcalc259");
  }
  {
    ASTCFGTester n(createCode("<e v='cover'>   \
                                <p v='inp5s.map'/>        \
                                <p p='8' v='inp1b.map'/>  \
                           </e>"));
    DEFAULT_BTV;
    EXPECT_ERROR(btv,n,"pcrcalc259a");
  }
  {
    ASTCFGTester n(createCode("<e v='eq'>                        \
                                <p v='inp1b.map'/>           \
                                <e v='nominal' p='8'><n v='4'/></e> \
                           </e>"));
    DEFAULT_BTV;
    EXPECT_ERROR(btv,n,"pcrcalc260");
  }
  {
    ASTCFGTester n(createFromId("pcrcalc260a"));
    DEFAULT_BTV;
    EXPECT_ERROR(btv,n,"pcrcalc260a");
  }
}

void calc::BuildTypesVisitorTest::testAssError()
{
  {
    ASTCFGTester n(createFromId("pcrcalc2a"));
    DEFAULT_BTV;
    EXPECT_ERROR(btv,n,"pcrcalc2a");
  }
  {
    ASTCFGTester n(createFromId("pcrcalc2"));
    DEFAULT_BTV;
    EXPECT_ERROR(btv,n,"pcrcalc2");
  }
  {
    // 1st spread narrows useOnly to VS_BNO
    // 2nd spread expects useOnly to be VS_S
    ASTCFGTester n(createCode("<l><e v='spread'>       \
                                <p v='useOnly' p='firstUse'/>  \
                                <p v='x'/>                     \
                                <n v='1'/>                     \
                           </e><e v='spread'>                  \
                                <p v='y'/>                     \
                                <p v='useOnly' p='2ndUse' />   \
                                <n p='1'        v='1'/>        \
                           </e></l>"));
    DEFAULT_BTV;
    EXPECT_ERROR(btv,n,"pcrcalc501"); // want something else see messages.txt
  }
}

void calc::BuildTypesVisitorTest::testNonFieldError()
{
  {
    ASTCFGTester n(createFromId("pcrcalc44"));
    DEFAULT_BTV;
    EXPECT_ERROR(btv,n,"pcrcalc44");
  }
  {
    ASTCFGTester n(createFromId("pcrcalc257"));
    DEFAULT_BTV;
    EXPECT_ERROR(btv,n,"pcrcalc257");
  }
  {
    ASTCFGTester n(createFromId("pcrcalc257a"));
    DEFAULT_BTV;
    EXPECT_ERROR(btv,n,"pcrcalc257a");
  }
  {
    ASTCFGTester n(createFromId("pcrcalc502"));
    DEFAULT_BTV;
    EXPECT_ERROR(btv,n,"pcrcalc502");
  }
  {
    ASTCFGTester n(createFromId("pcrcalc503"));
    DEFAULT_BTV;
    EXPECT_ERROR(btv,n,"pcrcalc503");
  }
}

//! type info that can only be deduced by pushing type info down
void calc::BuildTypesVisitorTest::testTopDownExprRestrictor()
{
  {
    // multiply by 1 must push VS_S as resultType of stackA
    const char *code="timer 1 1 1; dynamic a=1*timeinput(stackA)";
    ASTCFGTester n(StringParser::createCodeAsNode(code));
    DEFAULT_BTV;
    btv.visit();
    BOOST_CHECK(btv.table().contains("stackA"));
    DataType dt(btv.table()["stackA"].dataType());
    BOOST_CHECK(dt.vs()         == VS_MAPSTACK);
    BOOST_CHECK(dt.resultType() == VS_S);
  }
  {
    const char *code="t=1*(if(e,a,if(e2,b,c)))";
    ASTCFGTester n(StringParser::createCodeAsNode(code));
    DEFAULT_BTV;
    size_t nrChanges;
    do {
        nrChanges=btv.nrChanges();
        btv.visit();
    } while (btv.nrChanges()!=nrChanges && nrChanges < 10);

    const char *v[3]={"a","b","c"};
    for(size_t i=0;i<3;++i) {
      BOOST_CHECK(btv.table().contains(v[i]));
      BOOST_CHECK(btv.table()[v[i]].dataType().vs() == VS_S);
    }
    BOOST_CHECK(btv.table().contains("t"));
    BOOST_CHECK(btv.table()["t"].dataType().vs()==VS_S);
    BOOST_CHECK(btv.table()["t"].dataType().stEither());
  }
  {
    const char s[]="t= succ(4) eq if(e , a , if ( e2 , b , c))";
    ASTCFGTester n(StringParser::createCodeAsNode(s));
    DEFAULT_BTV;
    btv.visit();
    const char *v[3]={"a","b","c"};
    for(size_t i=0;i<3;++i) {
      BOOST_CHECK(btv.table().contains(v[i]));
      BOOST_CHECK(btv.table()[v[i]].dataType().vs() == VS_O);
      BOOST_CHECK(btv.table()[v[i]].dataType().stEither());
    }
    BOOST_CHECK(btv.table().contains("t"));
    BOOST_CHECK(btv.table()["t"].dataType().vs()==VS_B);
    BOOST_CHECK(btv.table()["t"].dataType().stEither());
  }
  { // succ(4) is VS_O, push  down VS_O to s
    const char s[]="timer 1 1 1; dynamic t= succ(4) eq if(e , a , if ( e2 , b , timeinput(s)))";
    ASTCFGTester n(StringParser::createCodeAsNode(s));
    DEFAULT_BTV;
    btv.visit();
    BOOST_CHECK(btv.table().contains("s"));
    DataType dt(btv.table()["s"].dataType());
    BOOST_CHECK(dt.vs()        == VS_MAPSTACK);
    BOOST_CHECK(dt.resultType()== VS_O);
  }
  { // setKeyTypes test
    ASTCFGTester n(StringParser::createExpr("lookupscalar(A,1*4,max(1,0))"));
    DEFAULT_BTV;
    btv.visit();
    BOOST_CHECK(btv.table().contains("A"));
    DataType dt(btv.table()["A"].dataType());
    BOOST_CHECK(dt.vs() == VS_TABLE);
    BOOST_CHECK(dt.tableColTypes().size()== 3);
    BOOST_CHECK(dt.tableColTypes()[0]    == VS_S);
    BOOST_CHECK(dt.tableColTypes()[1]    == VS_OS);
    BOOST_CHECK(dt.tableColTypes()[2]    == VS_S);
  }
}

void calc::BuildTypesVisitorTest::testMultipleVisits()
{

  ASTCFGTester n(createFromId("pcrcalc60"));
  BuildTypesVisitor btv(n.cfg());
  // a static model that has this problem can ook met variable
  // renaming wat dus nie kan met een dynamic model omdat er 2 paths
  // naar een use zijn.

  size_t nrChanges=btv.nrChanges();
  size_t nrLoops(0);
  do {
   nrChanges=btv.nrChanges();
   try {
      btv.visit();
   } catch(const DataTypeClash& ) {
     BOOST_CHECK(false);
   }
   const ASTSymbolTable& t(btv.table());
   BOOST_CHECK(t.contains("DSt"));
   BOOST_CHECK(t.contains("DSt0"));

   switch (nrLoops) {
     case 0:
      BOOST_CHECK(t["DSt"].dataType().st()  == ST_NONSPATIAL);
      BOOST_CHECK(t["DSt0"].dataType().st() == ST_NONSPATIAL);
      break;
     case 1:
      BOOST_CHECK(t["DSt"].dataType().st()  == ST_SPATIAL);
      BOOST_CHECK(t["DSt0"].dataType().st() == ST_NONSPATIAL);
      break;
     case 2:
     case 3:
      BOOST_CHECK(t["DSt"].dataType().st()  == ST_SPATIAL);
      BOOST_CHECK(t["DSt0"].dataType().st() == ST_SPATIAL);
      break;
   }
    nrLoops++;
  } while((nrChanges != btv.nrChanges() || nrLoops <=1)
           && nrLoops < 8); // MUST TERMINATE!
  BOOST_CHECK(nrLoops == 4);
}


/*
 * number type casting
 *  test342
 *  test83
 */
void calc::BuildTypesVisitorTest::testNumberTyping()
{
  { // BuildTypesVisitor does alter ASTNumber::d_vs
    ASTNumber *nr=createNumber("1");
    BOOST_CHECK(nr->vs() == VS_FIELD);
    ASTExpr   *e=createExpr("+");
    e->transferArg(createPar("inp1s.map"));
    e->transferArg(nr);

    ASTCFGTester n(e);
    DEFAULT_BTV;
    btv.visit();
    BOOST_CHECK(nr->vs() == VS_S);
  }
  {
    std::string msgId("pcrcalc214c");
    ASTCFGTester n(createFromId(msgId));
    BuildTypesVisitor btv(n.cfg());
    ASTNumber *nr= astCast<ASTNumber>(n.ast(),"C/b/0/a/>/,/1/n");
    BOOST_CHECK(nr->value()==5);

    btv.visit();
    BOOST_CHECK(nr->vs()  !=VS_S);

    btv.visit();
    BOOST_CHECK(nr->vs()  ==VS_S);
  }
}

void calc::BuildTypesVisitorTest::testDoubleFuncRelic()
{
  {
    std::string msgId("pcrcalc11b");
    bool  catched=false;
    try {
      ASTCFGTester n(createFromId(msgId));
      DEFAULT_BTV;
      btv.visit();
    } catch (const com::Exception& s) {
      BOOST_CHECK(msgVerify(msgId,s));
      catched=true;
    }
    BOOST_CHECK(catched);
  }
  {
    std::string msgId("pcrcalc11c");
    bool  catched=false;
    try {
      ASTCFGTester n(createFromId(msgId));
      DEFAULT_BTV;
      btv.visit();
    } catch (const com::Exception& s) {
      BOOST_CHECK(msgVerify(msgId,s));
      catched=true;
    }
    BOOST_CHECK(catched);
  }
  {
    bool  catched=false;
    std::string msgId("pcrcalc101");
    try {
      ASTCFGTester n(createFromId(msgId));
      DEFAULT_BTV;
      btv.visit();
    } catch (const com::Exception& s) {
      BOOST_CHECK(msgVerify(msgId,s));
      catched=true;
    }
    BOOST_CHECK(catched);
  }
  { // pcrcalc11
    ASTCFGTester n(
        StringParser::createCodeAsNode("D,Z=spread,spreadzone(inp1b.map,0,1);"));
    DEFAULT_BTV;
    btv.visit();

    const ASTSymbolTable& t(btv.table());
    BOOST_CHECK(t.contains("inp1b.map"));
    BOOST_CHECK(t.contains("D"));
    BOOST_CHECK(t["D"].dataType().st()  == ST_SPATIAL);
    BOOST_CHECK(t["D"].dataType().vs()  == VS_S);
    BOOST_CHECK(btv.table().contains("Z"));
    BOOST_CHECK(t["Z"].dataType().st()  == ST_SPATIAL);
    BOOST_CHECK(t["Z"].dataType().vs()  == VS_B);

    // test if it ever apear left hand side
    BOOST_CHECK( t["Z"].d_firstAss);
    BOOST_CHECK(!t["inp1b.map"].d_firstAss);
  }
  { // pcrcalc11
    ASTCFGTester n(
        StringParser::createCodeAsNode("Z,D=spreadzone,spread(inp1b.map,0,1);"));
    DEFAULT_BTV;
    btv.visit();

    const ASTSymbolTable& t(btv.table());
    BOOST_CHECK(t.contains("inp1b.map"));
    BOOST_CHECK(t.contains("D"));
    BOOST_CHECK(t["D"].dataType().st()  == ST_SPATIAL);
    BOOST_CHECK(t["D"].dataType().vs()  == VS_S);
    BOOST_CHECK(btv.table().contains("Z"));
    BOOST_CHECK(t["Z"].dataType().st()  == ST_SPATIAL);
    BOOST_CHECK(t["Z"].dataType().vs()  == VS_B);
  }
  { // dynwave
    std::string msgId("pcrcalc507");
    ASTCFGTester n(createFromId(msgId));
    DEFAULT_BTV;

    btv.visit();

    const ASTSymbolTable& t(btv.table());
    BOOST_CHECK(t.contains("tmp507.tbl"));
    DataType dt(t["tmp507.tbl"].dataType());
    BOOST_CHECK(dt.vs()  == VS_TABLE);
    BOOST_CHECK(dt.tableColTypes().size()  == 4);
    BOOST_CHECK(dt.tableColTypes()[0]  == VS_NO);
    BOOST_CHECK(dt.tableColTypes()[1]  == VS_S);
    BOOST_CHECK(dt.tableColTypes()[2]  == VS_S);
    BOOST_CHECK(dt.tableColTypes()[3]  == VS_S);
  }
}

void calc::BuildTypesVisitorTest::testRepeat()
{
    ASTCFGTester n(
        StringParser::createCodeAsNode(
 "tmp.res = scalar(0); repeat { tmp.res += 5; } until 1;"));
    BuildTypesVisitor btv(n.cfg());
    bool todoAddBoolTestPath=false;
    BOOST_WARN(todoAddBoolTestPath);
}

