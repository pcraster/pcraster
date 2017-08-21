#define BOOST_TEST_MODULE pcraster model_engine buildtypesvisitor
#include <boost/test/unit_test.hpp>
#include <vector>
#include "com_exception.h"
#include "calc_astcfgtester.h"
#include "calc_astnumber.h"
#include "calc_positionname.h"
#include "calc_astexpr.h"
#include "calc_stringparser.h"
#include "calc_astpath.h"
#include "calc_datatypeclash.h"
#include "calc_asttestfactory.h"
#include "calc_datatype.h"
#include "calc_LibraryClass.h"

#define private public
#include "calc_astpar.h"
#include "calc_buildtypesvisitor.h"


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


struct Fixture : public calc::LibraryClassNoQt
{

  void insertTestTable(
    calc::ASTSymbolTable *d_inputTable,
    std::vector<calc::ASTPar *>   d_tt_pars,
    const std::string& name,
    const calc::DataType& ft)
  {
    using namespace calc;

    ASTTestFactory tmp_ast;
    ASTPar *p=tmp_ast.createPar(name);
    d_tt_pars.push_back(p); // for deletion
    PositionName pn("testTable");
    p->setPosition(&pn);
    (*d_inputTable)[p].dataType()=ft;
    // act as if name is created somewhere first
    (*d_inputTable)[p].setIoType(IOType(
      pcrxml::ModelInputType::None,pcrxml::ModelOutputType::Fixed));
  }


  Fixture() : calc::LibraryClassNoQt("buildtypesvisitor")
  {
    using namespace calc;
    d_inputTable = new ASTSymbolTable();

    insertTestTable(d_inputTable, d_tt_pars, "inp1s.map" ,DataType(VS_S,ST_SPATIAL ));
    insertTestTable(d_inputTable, d_tt_pars, "inp90d.map" ,DataType(VS_D,ST_SPATIAL ));
    insertTestTable(d_inputTable, d_tt_pars, "inp5s.map" ,DataType(VS_S,ST_SPATIAL ));
    insertTestTable(d_inputTable, d_tt_pars, "inp1b.map" ,DataType(VS_B,ST_SPATIAL ));
    insertTestTable(d_inputTable, d_tt_pars, "inp1n.map" ,DataType(VS_N,ST_SPATIAL ));
    insertTestTable(d_inputTable, d_tt_pars, "inpxo.map" ,DataType(VS_O,ST_SPATIAL ));
  }


  ~Fixture()
  {
    delete d_inputTable;
    d_inputTable=0;
    for(size_t i=0; i< d_tt_pars.size(); ++i)
      delete d_tt_pars[i];
    d_tt_pars.clear();
  }

  calc::ASTSymbolTable *d_inputTable;
  std::vector<calc::ASTPar *>   d_tt_pars;

};


BOOST_FIXTURE_TEST_SUITE(buildtypesvisitor, Fixture)


BOOST_AUTO_TEST_CASE(testPar)
{
  using namespace calc;

  ASTTestFactory tmp_ast;

 { // get type
   ASTPar *p= tmp_ast.createPar("inp1s.map");
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
   ASTPar *p= tmp_ast.createPar("inp1s.map");
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
   ASTCFGTester n(tmp_ast.createPar("anything"));
   BuildTypesVisitor btv(n.cfg());

   btv.visit();
   DataType btvRes(n.ast()->returnDataType());
   BOOST_CHECK(btvRes.vs() == VS_ANYTHING);
 }

}

BOOST_AUTO_TEST_CASE(testNumber)
{
  using namespace calc;

  ASTTestFactory tmp_ast;
  ASTCFGTester n(tmp_ast.createNumber("0.5"));
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

BOOST_AUTO_TEST_CASE(testModel)
{
  using namespace calc;

  ASTTestFactory tmp_ast;
  // misc tests on a whole script
  {
   ASTCFGTester n(tmp_ast.createFromId("pcrcalc376"));
   BuildTypesVisitor btv(n.cfg());
   // no visit no dynamic section found
   BOOST_CHECK(!btv.containsDynamicSection());
  }
  { // this is not the error message test for 376
    ASTCFGTester n(tmp_ast.createFromId("pcrcalc376"));
    BuildTypesVisitor btv(n.cfg());
   // visit, dynamic section found
    btv.visit();
    BOOST_CHECK(btv.containsDynamicSection());
  }
  {
    ASTCFGTester n(tmp_ast.createFromId("pcrcalc60"));
    BuildTypesVisitor btv(n.cfg());
    btv.visit();
    BOOST_CHECK(!btv.containsDynamicSection());
  }
  {
    ASTCFGTester n(tmp_ast.createFromId("pcrcalc505"));
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
    ASTCFGTester n(tmp_ast.createFromId("pcrcalc532"));
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

BOOST_AUTO_TEST_CASE(testExpr)
{
  using namespace calc;

  ASTTestFactory tmp_ast;

  {
    ASTCFGTester n(tmp_ast.createCode("<e v='+'>       \
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
    ASTCFGTester n(tmp_ast.createCode("<e v='mapuniform'> \
                           </e>"));

    DEFAULT_BTV;
    btv.visit();
    DataType btvRes(n.ast()->returnDataType());
    BOOST_CHECK(btvRes.vs() == VS_S);
    BOOST_CHECK(btvRes.stNonSpatial());
  }
  { // poly arguments
    // st can be both
    ASTCFGTester n(tmp_ast.createCode("<e v='mapmaximum'> \
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
    ASTCFGTester n(tmp_ast.createCode("<e v='spreadzone' p='8'>   \
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
    ASTCFGTester n(tmp_ast.createCode("<e v='nodirection'>       \
                                <p v='inp90d.map'/>    \
                               </e>"));
    DEFAULT_BTV;
    btv.visit();
    DataType btvRes(n.ast()->returnDataType());
    BOOST_CHECK(btvRes.vs() == VS_B);
    BOOST_CHECK(btvRes.stSpatial());
  }
  {
    ASTCFGTester n(tmp_ast.createCode("<e v='nodirection'>       \
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

BOOST_AUTO_TEST_CASE(testReportParsed)
{
  using namespace calc;

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

BOOST_AUTO_TEST_CASE(testErrorExpr)
{
  using namespace calc;


  ASTTestFactory tmp_ast;

  {
    ASTCFGTester n(tmp_ast.createCode("<e v='spread' p='8'>   \
                                <n v='1'/>        \
                                <n v='1'/>        \
                                <n v='1'/>        \
                           </e>"));
    DEFAULT_BTV;
    EXPECT_ERROR(btv,n,"pcrcalc258");
  }
  {
    ASTCFGTester n(tmp_ast.createCode("<e v='spread' p='8'>   \
                                <n p='9' v='1'/>  \
                                <n v='1'/>        \
                           </e>"));
    DEFAULT_BTV;
    EXPECT_ERROR(btv,n,"pcrcalc252");
  }
  {
    ASTCFGTester n(tmp_ast.createCode("<e v='maparea' p='8'/>"));
    DEFAULT_BTV;
    EXPECT_ERROR(btv,n,"pcrcalc252a");
  }
  {
    ASTCFGTester n(tmp_ast.createCode("<e v='spread' p='8'>   \
                                <n p='9' v='1'/>  \
                                <n v='1'/>        \
                                <n v='1'/>        \
                                <n v='1'/>        \
                           </e>"));
    DEFAULT_BTV;
    EXPECT_ERROR(btv,n,"pcrcalc253");
  }
  {
    ASTCFGTester n(tmp_ast.createCode("<e v='spread' p='8'>   \
                             <e v='+'><n v='1'/><n v='1'/></e> \
                             <n v='1'/>       \
                             <n v='1'/>       \
                           </e>"));
    DEFAULT_BTV;
    EXPECT_ERROR(btv,n,"pcrcalc500");
  }
  { // modellink
    ASTCFGTester n(tmp_ast.createFromId("pcrcalc548"));
    DEFAULT_BTV;
    EXPECT_ERROR(btv,n,"pcrcalc548");
  }
  { // unknown function
    ASTCFGTester n(tmp_ast.createFromId("pcrcalc549"));
    DEFAULT_BTV;
    EXPECT_ERROR(btv,n,"pcrcalc549");
  }
  { // unknown method
    ASTCFGTester n(tmp_ast.createFromId("pcrcalc550"));
    DEFAULT_BTV;
    EXPECT_ERROR(btv,n,"pcrcalc550");
  }
  if (0) { // nog niet noodzakelijk
    ASTCFGTester n(tmp_ast.createFromId("pcrcalc102a"));
    DEFAULT_BTV;
    EXPECT_ERROR(btv,n,"pcrcalc102a");
  }
}

BOOST_AUTO_TEST_CASE(testArgCombError)
{
  using namespace calc;

  ASTTestFactory tmp_ast;

  {
    ASTCFGTester n(tmp_ast.createCode("<e v='cover'>   \
                                <p v='inp5s.map'/>        \
                                <p v='inp5s.map'/>        \
                                <p p='8' v='inp1b.map'/>  \
                           </e>"));
    DEFAULT_BTV;
    EXPECT_ERROR(btv,n,"pcrcalc259");
  }
  {
    ASTCFGTester n(tmp_ast.createCode("<e v='cover'>   \
                                <p v='inp5s.map'/>        \
                                <p p='8' v='inp1b.map'/>  \
                           </e>"));
    DEFAULT_BTV;
    EXPECT_ERROR(btv,n,"pcrcalc259a");
  }
  {
    ASTCFGTester n(tmp_ast.createCode("<e v='eq'>                        \
                                <p v='inp1b.map'/>           \
                                <e v='nominal' p='8'><n v='4'/></e> \
                           </e>"));
    DEFAULT_BTV;
    EXPECT_ERROR(btv,n,"pcrcalc260");
  }
  {
    ASTCFGTester n(tmp_ast.createFromId("pcrcalc260a"));
    DEFAULT_BTV;
    EXPECT_ERROR(btv,n,"pcrcalc260a");
  }
}

BOOST_AUTO_TEST_CASE(testAssError)
{
  using namespace calc;

  ASTTestFactory tmp_ast;

  {
    ASTCFGTester n(tmp_ast.createFromId("pcrcalc2a"));
    DEFAULT_BTV;
    EXPECT_ERROR(btv,n,"pcrcalc2a");
  }
  {
    ASTCFGTester n(tmp_ast.createFromId("pcrcalc2"));
    DEFAULT_BTV;
    EXPECT_ERROR(btv,n,"pcrcalc2");
  }
  {
    // 1st spread narrows useOnly to VS_BNO
    // 2nd spread expects useOnly to be VS_S
    ASTCFGTester n(tmp_ast.createCode("<l><e v='spread'>       \
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

BOOST_AUTO_TEST_CASE(testNonFieldError)
{
  using namespace calc;

  ASTTestFactory tmp_ast;

  {
    ASTCFGTester n(tmp_ast.createFromId("pcrcalc44"));
    DEFAULT_BTV;
    EXPECT_ERROR(btv,n,"pcrcalc44");
  }
  {
    ASTCFGTester n(tmp_ast.createFromId("pcrcalc257"));
    DEFAULT_BTV;
    EXPECT_ERROR(btv,n,"pcrcalc257");
  }
  {
    ASTCFGTester n(tmp_ast.createFromId("pcrcalc257a"));
    DEFAULT_BTV;
    EXPECT_ERROR(btv,n,"pcrcalc257a");
  }
  {
    ASTCFGTester n(tmp_ast.createFromId("pcrcalc502"));
    DEFAULT_BTV;
    EXPECT_ERROR(btv,n,"pcrcalc502");
  }
  {
    ASTCFGTester n(tmp_ast.createFromId("pcrcalc503"));
    DEFAULT_BTV;
    EXPECT_ERROR(btv,n,"pcrcalc503");
  }
}

//! type info that can only be deduced by pushing type info down
BOOST_AUTO_TEST_CASE(testTopDownExprRestrictor)
{
  using namespace calc;

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

BOOST_AUTO_TEST_CASE(testMultipleVisits)
{
  using namespace calc;

  ASTTestFactory tmp_ast;

  ASTCFGTester n(tmp_ast.createFromId("pcrcalc60"));
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
BOOST_AUTO_TEST_CASE(testNumberTyping)
{
  using namespace calc;

  ASTTestFactory tmp_ast;

  { // BuildTypesVisitor does alter ASTNumber::d_vs
    ASTNumber *nr=tmp_ast.createNumber("1");
    BOOST_CHECK(nr->vs() == VS_FIELD);
    ASTExpr   *e=tmp_ast.createExpr("+");
    e->transferArg(tmp_ast.createPar("inp1s.map"));
    e->transferArg(nr);

    ASTCFGTester n(e);
    DEFAULT_BTV;
    btv.visit();
    BOOST_CHECK(nr->vs() == VS_S);
  }
  {
    std::string msgId("pcrcalc214c");
    ASTCFGTester n(tmp_ast.createFromId(msgId));
    BuildTypesVisitor btv(n.cfg());
    ASTNumber *nr= astCast<ASTNumber>(n.ast(),"C/b/0/a/>/,/1/n");
    BOOST_CHECK(nr->value()==5);

    btv.visit();
    BOOST_CHECK(nr->vs()  !=VS_S);

    btv.visit();
    BOOST_CHECK(nr->vs()  ==VS_S);
  }
}

BOOST_AUTO_TEST_CASE(testDoubleFuncRelic)
{
  using namespace calc;

  ASTTestFactory tmp_ast;

  {
    std::string msgId("pcrcalc11b");
    bool  catched=false;
    try {
      ASTCFGTester n(tmp_ast.createFromId(msgId));
      DEFAULT_BTV;
      btv.visit();
    } catch (const com::Exception& s) {
      BOOST_CHECK(tmp_ast.msgVerify(msgId,s));
      catched=true;
    }
    BOOST_CHECK(catched);
  }
  {
    std::string msgId("pcrcalc11c");
    bool  catched=false;
    try {
      ASTCFGTester n(tmp_ast.createFromId(msgId));
      DEFAULT_BTV;
      btv.visit();
    } catch (const com::Exception& s) {
      BOOST_CHECK(tmp_ast.msgVerify(msgId,s));
      catched=true;
    }
    BOOST_CHECK(catched);
  }
  {
    bool  catched=false;
    std::string msgId("pcrcalc101");
    try {
      ASTCFGTester n(tmp_ast.createFromId(msgId));
      DEFAULT_BTV;
      btv.visit();
    } catch (const com::Exception& s) {
      BOOST_CHECK(tmp_ast.msgVerify(msgId,s));
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
    ASTTestFactory tmp_ast;
    std::string msgId("pcrcalc507");
    ASTCFGTester n(tmp_ast.createFromId(msgId));
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

BOOST_AUTO_TEST_CASE(testRepeat)
{
  using namespace calc;

    ASTCFGTester n(
        StringParser::createCodeAsNode(
 "tmp.res = scalar(0); repeat { tmp.res += 5; } until 1;"));
    BuildTypesVisitor btv(n.cfg());
    bool todoAddBoolTestPath=false;
    BOOST_WARN(todoAddBoolTestPath);
}


BOOST_AUTO_TEST_SUITE_END()
