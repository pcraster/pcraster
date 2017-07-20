#define BOOST_TEST_MODULE pcraster model_engine executor
#include <boost/test/unit_test.hpp>
#include "calc_asttestfactory.h"
#include "geo_filecreatetester.h"
#include "com_csfcell.h"
#include "com_directory.h"
#include "com_file.h"
#include "calc_executor.h"
#include "calc_field.h"
#include "calc_p5stack.h"
#include "calc_dvautoptr.h"
#include "calc_globallibdefs.h"
#include "calc_spatial.h"
#include "calc_globallibdefs.h"

struct Fixture
{

    Fixture()
    {
        calc::globalInit();
    }


    ~Fixture()
    {
        calc::globalEnd();
    }

};

BOOST_GLOBAL_FIXTURE(Fixture);

BOOST_AUTO_TEST_CASE(testSetStep)
{
  using namespace calc;

  { // 1: both initial and dynamic
    const char *code = " timer 1 2 1; \
                         initial  \
                          a = 1; \
                         dynamic \
                          tmp.res=timeoutput(nominal(inp5s.map),time()*10); \
                          b      =inp5s.map+time()*a; ";
    std::auto_ptr<ASTScript>  s(ASTTestFactory::createFromIdOrStr(code));
    s->analyzeAndResolve();
    Executor e(s->cfgCode(),s->rteSettings(),s->symbols());

    e.startStepWise();
    BOOST_CHECK(0 == e.runTimeEnv().timer().currentInt());
    e.execInitialSection();
    BOOST_CHECK(1 == e.runTimeEnv().timer().currentInt());
    size_t t=1;
    do {
      BOOST_CHECK(e.runTimeEnv().timer().currentInt() == t);
      t++;
    } while(!e.execDynamicSectionOnce());
    BOOST_CHECK(t==3);
    e.finishStepWise();
    BOOST_CHECK(ASTTestFactory::fileVerify("pcrcalc249","tmp.res"));
  }
  { // 1: only dynamic
    const char *code = " timer 1 2 1; \
                         dynamic \
                          tmp.res=timeoutput(nominal(inp5s.map),time()*10); \
                          b      =inp5s.map+time(); ";
    std::auto_ptr<ASTScript>  s(ASTTestFactory::createFromIdOrStr(code));
    s->analyzeAndResolve();
    Executor e(s->cfgCode(),s->rteSettings(),s->symbols());

    e.startStepWise();
    BOOST_CHECK(0 == e.runTimeEnv().timer().currentInt());
    e.execInitialSection(); // does not exec anything
    BOOST_CHECK(1 == e.runTimeEnv().timer().currentInt());
    size_t t=1;
    do {
      BOOST_CHECK(e.runTimeEnv().timer().currentInt() == t);
      t++;
    } while(!e.execDynamicSectionOnce());
    BOOST_CHECK(3==t);
    // will do nothing
    BOOST_CHECK(e.execDynamicSectionOnce());
    BOOST_CHECK(2 == e.runTimeEnv().timer().currentInt());
    e.finishStepWise();
    BOOST_CHECK(ASTTestFactory::fileVerify("pcrcalc249","tmp.res"));
  }

  { // 1: only initial
    const char *code = "tmp.res=inp5s.map*1;";
    std::auto_ptr<ASTScript>  s(ASTTestFactory::createFromIdOrStr(code));
    s->analyzeAndResolve();
    Executor e(s->cfgCode(),s->rteSettings(),s->symbols());

    geo::FileCreateTester fct("tmp.res");

    e.startStepWise();
    BOOST_CHECK(0 == e.runTimeEnv().timer().currentInt());
    e.execInitialSection(); // does exec everything
    BOOST_CHECK(0 == e.runTimeEnv().timer().currentInt());
    BOOST_CHECK(e.execDynamicSectionOnce());
    BOOST_CHECK(0 == e.runTimeEnv().timer().currentInt());
    BOOST_CHECK(e.execDynamicSectionOnce());

    e.finishStepWise();
    BOOST_CHECK(fct.equalTo("inp5s.map",false));
  }
}

BOOST_AUTO_TEST_CASE(testErrors)
{
  using namespace calc;

  EXEC_ERROR_TEST("pcrcalc82");
  EXEC_ERROR_TEST("pcrcalc248a"); // DROPPED boolean requirement
  EXEC_ERROR_TEST("pcrcalc4");
  EXEC_ERROR_TEST("pcrcalc5a");
  EXEC_ERROR_TEST("pcrcalc5c");

  EXEC_ERROR_TEST("pcrcalc6");
  EXEC_ERROR_TEST("pcrcalc7");
  EXEC_ERROR_TEST("pcrcalc7a");
  EXEC_ERROR_TEST("pcrcalc8");
  EXEC_ERROR_TEST("pcrcalc8a");
  EXEC_ERROR_TEST("pcrcalc8ab");
  EXEC_ERROR_TEST("pcrcalc8b");

  EXEC_ERROR_TEST("pcrcalc69");

  EXEC_ERROR_TEST("pcrcalc352");
  EXEC_ERROR_TEST("pcrcalc355");

  EXEC_ERROR_TEST("pcrcalc37c");

  EXEC_ERROR_TEST("pcrcalc45");
  EXEC_ERROR_TEST("pcrcalc46");

  EXEC_ERROR_TEST("pcrcalc38");
  EXEC_ERROR_TEST("pcrcalc38a");
  EXEC_ERROR_TEST("pcrcalc68");
  EXEC_ERROR_TEST("pcrcalc504");
  EXEC_ERROR_TEST("pcrcalc534");
  EXEC_ERROR_TEST("pcrcalc535");
  EXEC_ERROR_TEST("pcrcalc536");
  EXEC_ERROR_TEST("pcrcalc540");
  EXEC_ERROR_TEST("pcrcalc545");

  EXEC_ERROR_TEST("pcrcalc237");
  EXEC_ERROR_TEST("pcrcalc255");

  {
   com::write("1 1\n2 2","twoSteps.tss");

   EXEC_ERROR_TEST("pcrcalc226");
   EXEC_ERROR_TEST("pcrcalc232");
  }
  {
   com::ScopedCopy sc1("inp5s.map",   "tmp_s000.001");
   com::ScopedCopy sc2("twoSteps.tss","tmp_s000.002");
   EXEC_ERROR_TEST("pcrcalc250");
  }
  {
   com::ScopedCopy s1("inp5s.map", "tmp_s000.001");
   com::ScopedCopy s2("inp1b.map", "tmp_s000.002");
   EXEC_ERROR_TEST("pcrcalc250a");
  }
  {
   com::ScopedCopy s1("inp5s.map", "tmp_s000.003");
   com::ScopedCopy s2("inp1b.map", "tmp_s000.008");
   EXEC_ERROR_TEST("pcrcalc250c");
  }
  {
   com::ScopedCopy s1("sort1.map", "tmp_s000.001");
   com::ScopedCopy s2("inp1s.map", "tmp_s000.002");
   EXEC_ERROR_TEST("pcrcalc344");
   EXEC_ERROR_TEST("pcrcalc344a");
  }

  EXEC_ERROR_TEST("pcrcalc370");

  EXEC_ERROR_TEST("pcrcalc376");
  EXEC_ERROR_TEST("pcrcalc505");

  EXEC_ERROR_TEST("pcrcalc538");
  EXEC_ERROR_TEST("pcrcalc10a");

  {
   com::ScopedCopy s2("inp5s.map", "tmp_s000.001");
   com::ScopedCopy s1("inp1s.map", "tmp_s000.002");
   EXEC_ERROR_TEST("pcrcalc250f");
  }
  {
   com::ScopedCopy s1("inp5s.map", "tmp_lms0.001");
   // 1 existing others not
   EXEC_ERROR_TEST("pcrcalc541");
   EXEC_ERROR_TEST("pcrcalc543");
  }
  {
   com::ScopedCopy s1("inp5s.map", "tmp_lms0.001");
   com::ScopedCopy s2("inp1n.map", "tmp_lms0.002");
   EXEC_ERROR_TEST("pcrcalc542");
  }

  EXEC_ERROR_TEST("pcrcalc547");

  bool todo_pcrcalc364and539=false;
  BOOST_WARN(todo_pcrcalc364and539);
  // EXEC_ERROR_TEST("pcrcalc364");
  // EXEC_ERROR_TEST("pcrcalc539");
}

BOOST_AUTO_TEST_CASE(testRunTimeErrors)
{
  using namespace calc;

  EXEC_ERROR_TEST("pcrcalc225");

  {
   com::write("1 1e31\n","tmp37e.tss");
   EXEC_ERROR_TEST("pcrcalc37e");
  }

  EXEC_ERROR_TEST("pcrcalc247");
  EXEC_ERROR_TEST("pcrcalc248");

  EXEC_ERROR_TEST("pcrcalc561");

  bool todo_runTimeMsgAccuMRF=false;
  BOOST_WARN(todo_runTimeMsgAccuMRF);
  // see template<class AccuStateFo> static void calc::accuStateFlux()
  // also make error in 8th argument of dynwave
}

BOOST_AUTO_TEST_CASE(testDomainErrors)
{
  using namespace calc;

 /* runtime error, then clean-up of RunTimeStack on load of AtOutflow
  * in dtor clean of RunTimeStack
  */
  EXEC_ERROR_TEST("pcrcalc522");

  EXEC_ERROR_TEST("pcrcalc228");
  EXEC_ERROR_TEST("pcrcalc229");
  EXEC_ERROR_TEST("pcrcalc64a");

  EXEC_ERROR_TEST("pcrcalc337");
  EXEC_ERROR_TEST("pcrcalc337a");
  EXEC_ERROR_TEST("pcrcalc338");
  EXEC_ERROR_TEST("pcrcalc294");
  EXEC_ERROR_TEST("pcrcalc348");

  EXEC_ERROR_TEST("pcrcalc349");
  EXEC_ERROR_TEST("pcrcalc350");
  EXEC_ERROR_TEST("pcrcalc351");
}

BOOST_AUTO_TEST_CASE(testBugs)
{
  using namespace calc;


  // bug Feb/28/2005 no syms found within loop
  std::auto_ptr<ASTScript>s(ASTTestFactory::createFromIdOrStr("pcrcalc527"));
  s->analyzeAndResolve();
  Executor e(s->cfgCode(),s->rteSettings(),s->symbols());

    e.startStepWise();
    BOOST_CHECK(0 == e.runTimeEnv().timer().currentInt());
    e.execInitialSection();
    BOOST_CHECK(1 == e.runTimeEnv().timer().currentInt());
    size_t t=1;
    do {
      BOOST_CHECK(e.runTimeEnv().timer().currentInt() == t);
      t++;
    } while(!e.execDynamicSectionOnce());
    BOOST_CHECK(6==t);
    e.finishStepWise();



  bool todoSolveMultipleReads=false;
  BOOST_WARN(todoSolveMultipleReads);

#ifdef DEBUG_DEVELOP
  // since bug is harmless, only trap
  // it in develop mode

  // Multiple Reads
  //  EXEC_ERROR_TEST_WARN("pcrcalc523");
  //  EXEC_ERROR_TEST_WARN("pcrcalc525");

#endif
}

BOOST_AUTO_TEST_CASE(testExpr)
{
  using namespace calc;

  {
    P5Stack e("inp1s.map+inp5s.map");
    DVAutoPtr<Field> f(e.popResult());
    BOOST_CHECK(P5Stack::equal(f.get(),6));
  }
  { // test with multiple reference to the same Par
    P5Stack e("inp1s.map+inp1s.map+inp1s.map");
    DVAutoPtr<Field> f(e.popResult());
    BOOST_CHECK(P5Stack::equal(f.get(),3));
  }
  { // ASTNumber
    P5Stack e("inp1s.map+5");
    DVAutoPtr<Field> f(e.popResult());
    BOOST_CHECK(P5Stack::equal(f.get(),6));
  }
}

BOOST_AUTO_TEST_CASE(testLookup)
{
  using namespace calc;

  {
    P5Stack e("lookupnominal(inp_1.tbl,inp5s.map,8,10)");

    DVAutoPtr<Field> f(e.popResult());
    BOOST_CHECK(P5Stack::equal(f.get(),1,VS_N));
  }
  {
    P5Stack e("lookupnominal(inp_1.tbl,5,8,10)");
    DVAutoPtr<Field> f(e.popResult());
    BOOST_CHECK(P5Stack::equal(f.get(),1,VS_N,false));
  }
  {
    TestAsciiResult t;
    com::write("1 1\n5 5\n","tmp369.tbl");
    execTest("pcrcalc369");
    BOOST_CHECK(t.equals("pcrcalc369"));
  }
}

BOOST_AUTO_TEST_CASE(testAss)
{
  using namespace calc;

  {
    // Release build issue:
    /// using inp5s.map in the expr fails
    //  hence a copy.
    //  I suspect an open twice, but P5Stack
    //  is a test thing only, so leave it FTTB
    com::copy("inp5s.map","testAss.map");
    geo::FileCreateTester fct("A");
    P5Stack e("A=inp1s.map*testAss.map");

    BOOST_CHECK(e.contains("A"));
    BOOST_CHECK(e.equal("A",5));

    BOOST_CHECK(fct.equalTo("inp5s.map",true));
  }
  {
    geo::FileCreateTester fct("A");

    // test re-assignment of same parameter
    P5Stack e("A=4*inp1s.map;A=A+1");

    BOOST_CHECK(e.contains("A"));
    BOOST_CHECK(e.equal("A",5));

    BOOST_CHECK(fct.equalTo("inp5s.map",false));
  }
  {// pcrcalc11
   // double assigment
    // Release build issue:
    /// using inp1b.map in the expr twice fails
    //  hence a copy.
    //  I suspect an open twice, but P5Stack
    //  is a test thing only, so leave it FTTB
    com::copy("inp1b.map","testTwice.map");
    P5Stack e("Z,D=spreadzone,spread(inp1b.map==testTwice.map,0,1);");

    BOOST_CHECK(e.contains("D"));
    BOOST_CHECK(e.equal("D",0));

    BOOST_CHECK(e.contains("Z"));
    BOOST_CHECK(e.equal("Z",1,VS_B));
  }
  {// pcrcalc11  / reversed
   // double assigment
    P5Stack e("D,Z=spread,spreadzone(inp1b.map,3,1);");

    BOOST_CHECK(e.contains("D"));
    BOOST_CHECK(e.equal("D",3));

    BOOST_CHECK(e.contains("Z"));
    BOOST_CHECK(e.equal("Z",1,VS_B));
  }
  {
    execTest("pcrcalc519");
  }
}

BOOST_AUTO_TEST_CASE(testModel)
{
  using namespace calc;

  {
    geo::FileCreateTester fct("tmp.res");
    // test/pcrcalc5b clone as areamap
    execTest("areamap inp1s.map; initial tmp.res = 5*1;");

    BOOST_CHECK(fct.equalTo("inp5s_all.map",false));
  }
  { // check if binding creates the constant parameters
    geo::FileCreateTester fct("tmp.res");
    // test/pcrcalc5b clone as areamap
    execTest("binding constantNr=1;areamap inp1s.map; initial tmp.res = 5*constantNr;");

    BOOST_CHECK(fct.equalTo("inp5s_all.map",false));
  }
  { // check if binding see input map
    geo::FileCreateTester fct("tmp.res");
    // test/pcrcalc5b clone as areamap
    execTest("binding inpBinding=inp1s.map; initial tmp.res = 5*inpBinding;");

    BOOST_CHECK(fct.equalTo("inp5s.map",false));
  }
  { // check if binding makes output map
    geo::FileCreateTester fct("tmp.res");
    // test/pcrcalc5b clone as areamap
    execTest("binding outBinding=tmp.res; initial outBinding = 5*inp1s.map;");

    BOOST_CHECK(fct.equalTo("inp5s.map",false));
  }
  { // simpler then pcrcalc380, but same problem in DataTable::DTE::resetValue -> deref
    // UseDef analysis must keep inp1s.map alive
    geo::FileCreateTester fct("tmp.res");
    execTest("tmp.res=inp1s.map;tmp.res+=inp1s.map*0;");
    BOOST_CHECK(fct.equalTo("inp1s.map",false));
  }

  { // pcrcalc380
    // UseDef analysis must keep inp1s.map alive
    geo::FileCreateTester fct("tmp.res");
    execTest("tmp.res=inp1s.map;repeat{tmp.res+=inp1s.map;}until tmp.res eq 5;");
    BOOST_CHECK(fct.equalTo("inp5s.map",false));
  }
  { // pcrcalc379
    // exec repeat once
    geo::FileCreateTester fct("tmp.res");
    execTest("pcrcalc379");
    BOOST_CHECK(fct.equalTo("inp5s_all.map",false));
  }
  { // pcrcalc557
    // cast UINT1 to INT4
    geo::FileCreateTester fct("tmp557.res");
    execTest("pcrcalc557");
    BOOST_CHECK(fct.equalTo("inp1n.map",false));
  }
  { // min max on stack bug
    geo::FileCreateTester fct1("tmp56000.001");
    geo::FileCreateTester fct2("tmp56000.002");
    execTest("pcrcalc560");
    BOOST_CHECK(fct1.equalTo("res56000.001",false));
    BOOST_CHECK(fct2.equalTo("res56000.002",false));
  }
  // bugzilla #52
  { // lddcreate
    // TODO is anders op linux!
    geo::FileCreateTester fct("tmp559.res");
    execTest("pcrcalc559");
    BOOST_WARN(fct.equalTo("diagonalLdd559.map",false));
  }

  { // lddcreatend, TODO make example in pcrExamples.xml of manual
    //  DEM = WL inundatiekaart_2m.map
    //        Sun Nov 26 19:37:53 CET 2006
    // TODO is anders op linux!
    geo::FileCreateTester fct("tmp558.res");
    execTest("pcrcalc558");
    BOOST_WARN(fct.equalTo("nondiagonalLdd558.map",false));
  }
}

BOOST_AUTO_TEST_CASE(testRunDirectory)
{
  using namespace calc;

  {
    // pcrcalc353  write to existing dir
    com::PathName rd("tmpdirTestRunDirectory");
    com::createDirectory(rd);

    com::PathName res("tmp353.res");
    com::PathName resDir(rd+res);
    geo::FileCreateTester fct(resDir);

    // if present in cwd
    com::remove(res);

    execTest("pcrcalc353");

    // not present in cwd
    BOOST_CHECK(!com::exists(res));
    BOOST_CHECK(fct.equalTo("inp5s.map",true));

  }
  { //  areamap must adhere to -r
    //  -d will force searching for areamap

    com::PathName rd("tmpdirAreaMapInRunDirectory");
    com::createDirectory(rd);

    geo::FileCreateTester fct(rd+"tmp.res");

    com::PathName areamap(rd+"findit.map");
    BOOST_CHECK(!areamap.isEmpty());
    com::copy("inp1b.map",areamap);

    execTest("pcrcalc357a");
    BOOST_CHECK(fct.equalTo("inp5s_all.map",false));

  }
  {
    // write to new dir
    com::PathName rd("tmpdirNewTestRunDirectory");

    BOOST_CHECK(!com::exists(rd));

    com::PathName res("tmp.res");
    com::PathName resDir(rd+res);
    geo::FileCreateTester fct(resDir);

    // if present in cwd
    com::remove(res);

    execTest("pcrcalc354");

    BOOST_CHECK(fct.equalTo("inp5s.map",false));

    // not present in cwd
    BOOST_CHECK(!com::exists(res));
  }

#ifndef DEBUG_DEVELOP
  execTest("pcrcalc524");
#else
  {
    bool catched=false;
    try {
     execTest("pcrcalc524");
    } catch(const com::Exception& e) {
BOOST_CHECK(e.messages().find("IoFieldStrategy::createField") != std::string::npos);
       catched=true;
    }
    BOOST_CHECK(catched);
  }
#endif
}

BOOST_AUTO_TEST_CASE(testDynamic)
{
  using namespace calc;

  {
   // pcrcalc47
   // - timer must support binded symbol
   // - test nr of time steps taken
   // - non spatial report dynamic yields NSTssWriter tss
   TestAsciiResult res;
   execTest("binding e=3; areamap inp1b.map; timer 1 e 1; dynamic report tmp.res=time();");
   BOOST_CHECK(res.equals("pcrcalc47"));
  }
  {
   // pcrcalc47
   // - test loop feed back
   TestAsciiResult res;
   execTest("areamap inp1b.map; timer 1 3 1;initial tmp.res=0;dynamic report tmp.res+=1;");
   BOOST_CHECK(res.equals("pcrcalc47"));
  }
  {
    // pcrcalc214a
    geo::FileCreateTester s1("tmpStack.001");
    geo::FileCreateTester s2("tmpStack.002");
    geo::FileCreateTester s3("tmpStack.003");
    /* simple dynamic stack
     */
   execTest("pcrcalc214a");

   BOOST_CHECK(s1.equalTo("inp5s.map",false));
   BOOST_CHECK(s2.equalTo("inp5s.map",false));
   BOOST_CHECK(s3.equalTo("inp5s.map",false));
 }
  {
    // pcrcalc214b
    com::remove("tmpStack.001");
    com::remove("tmpStack.002");
    geo::FileCreateTester s3("tmpStack.003");
    /* sparse dynamic stack
     */
   execTest("pcrcalc214b");

   BOOST_CHECK(!com::exists("tmpStack.001"));
   BOOST_CHECK(!com::exists("tmpStack.002"));
   BOOST_CHECK(s3.equalTo("inp5s.map",false));
 }
  {
   // pcrcalc214c
   geo::FileCreateTester s("tmp.res");
   execTest("pcrcalc214c");

   BOOST_CHECK(s.equalTo("inp5s.map",false));
 }
  {
    com::remove("tmpStack");
    // pcrcalc214
    geo::FileCreateTester s1("tmpStack.001");
    geo::FileCreateTester s2("tmpStack.002");
    geo::FileCreateTester s3("tmpStack.003");
    /*
     * do we want this?
     * writing a nonspatial result to a map stack?
     */
   execTest("pcrcalc214");

   BOOST_CHECK(s1.equalTo("inp5s_all.map",false));
   BOOST_CHECK(s2.equalTo("inp5s_all.map",false));
   BOOST_CHECK(s3.equalTo("inp5s_all.map",false));
 }

 { // timeoutput statement
  TestAsciiResult res;
  execTest("pcrcalc249");
  BOOST_CHECK(res.equals("pcrcalc249"));
 }
 { // timeinput statement
  TestAsciiResult res;
  execTest("pcrcalc37d");
  BOOST_CHECK(res.equals("pcrcalc37d"));
 }
 {
   TestAsciiResult res;
   com::ScopedCopy s2("inp5s.map", "tmp_s000.001");
   com::ScopedCopy s1("inp1s.map", "tmp_s000.002");
   execTest("pcrcalc250b");
   BOOST_CHECK(res.equals("pcrcalc250b"));
 }
 {
   TestAsciiResult res;
   com::ScopedCopy s1("inp5s.map", "tmp_s000.004");
   com::ScopedCopy s2("inp1s.map", "tmp_s000.006");
   execTest("pcrcalc250d");
   BOOST_CHECK(res.equals("pcrcalc250d"));
 }
 {
   TestAsciiResult res;
   com::ScopedCopy s1("inp5s.map", "tmp_tim0.001");
   com::ScopedCopy s2("inp0s.map", "tmp_tim0.002");
   com::ScopedCopy s3("inp1s.map", "tmp_tim0.003");
   execTest("pcrcalc537");
   BOOST_CHECK(res.equals("pcrcalc537"));
 }
 {
   TestAsciiResult res;
   execTest("pcrcalc68a");
   BOOST_CHECK(res.equals("pcrcalc68a"));
 }
 {
   // test ICachedObject
   execTest("pcrcalc506");
 }
 {
   TestAsciiResult res;
   execTest("pcrcalc518");
   BOOST_CHECK(res.equals("pcrcalc518"));
 }
 { // d_readOnlyReferenceBug case 1
    geo::FileCreateTester fct("tmp.res");
    execTest("tmp.res = inp1s.map * inp1s.map;");
    BOOST_CHECK(fct.equalTo("inp1s.map",false));
  }
  { // d_readOnlyReferenceBug case 2
    geo::FileCreateTester fct("tmp.res");
    execTest("tmp.res = areamajority(inp1b.map,inp1b.map);");
    BOOST_CHECK(fct.equalTo("inp1b.map",false));
  }
  {
   TestAsciiResult res;
   execTest("pcrcalc317");
   BOOST_CHECK(res.equals("pcrcalc317"));
  }
  {
   TestAsciiResult res;
   execTest("pcrcalc13c");
   BOOST_CHECK(res.equals("pcrcalc13c"));
  }
  {
   TestAsciiResult res;
   execTest("pcrcalc13d");
   BOOST_CHECK(res.equals("pcrcalc13d"));
  }

  bool SeriousBindingClosureProblemPlusNovJrcReport=false;
  // this is the binding problem:
  // execTest("pcrcalc546");
  // assertion in calc_opimpl.cc
  // void calc::AreaTotal::exec ()
  //  .........
  //   PRECOND(args[0].nrValues()==args[1].nrValues());
  BOOST_WARN(SeriousBindingClosureProblemPlusNovJrcReport);
}

BOOST_AUTO_TEST_CASE(testUseDiskStorage)
{
  using namespace calc;

  try {
   Spatial::resetBPC();
   TestAsciiResult res;
   execTest("pcrcalc544");
   BOOST_CHECK(res.equals("pcrcalc544"));
   BOOST_CHECK(Spatial::maxBPC()==8);
  } catch(...) {
  }
}

BOOST_AUTO_TEST_CASE(testUseDef)
{
  using namespace calc;

 {
  // Due to field re-use this fails when lastUse's are set
  geo::FileCreateTester fct("tmpUseDef.res");
   execTest(
    "tmpUseDef.res=inp1s.map+4*(2-inp1s.map);\n") ;
   // 1 + 4*(2-1) = 5
   BOOST_CHECK(fct.equalTo("inp5s.map",false));
 }
 {
  // Due to field re-use this fails when lastUse's are set
  geo::FileCreateTester fct("tmpUseDef.res");
   execTest(
    "CF=inp1s.map;\n" \
    "tmpUseDef.res=CF+4*(2-CF);\n") ;
   // 1 + 4*(2-1) = 5
   BOOST_CHECK(fct.equalTo("inp5s.map",false));
 }
 {
  // Is until condition properly cleaned
  geo::FileCreateTester fct("tmpUseDef.res");
   execTest(
    "CF=inp1s.map;\n" \
    "repeat { report tmpUseDef.res=CF+4*(2-CF); } until (CF==1);\n") ;
   // 1 + 4*(2-1) = 5
   BOOST_CHECK(fct.equalTo("inp5s.map",false));
 }
}

BOOST_AUTO_TEST_CASE(testExternalBindings)
{
  using namespace calc;

  com::PathName pn("tmp.binding");
  {
    geo::FileCreateTester fct("tmp.res");
    com::write("Mul = 1;Result= tmp.res;",pn);
    execTest("pcrcalc371");
    BOOST_CHECK(fct.equalTo("inp1s.map",false));
  }
  {
    com::write("Mul = 1;Input=failureExpected372.map",pn);
    EXEC_ERROR_TEST("pcrcalc372");
  }
  {
    com::write("Input=inp1n.map",pn);
    EXEC_ERROR_TEST("pcrcalc372a");
  }
  {
    geo::FileCreateTester fct("tmp.res");
    com::write("Mul = inp5s.map;Result= tmp.res;",pn);
    execTest("pcrcalc373");
    BOOST_CHECK(fct.equalTo("inp5s.map",false));
  }
  {
    com::remove("failureExpected.map");
    geo::FileCreateTester fct("tmp.res");
    com::write("Mul = 5;",pn);
    execTest("pcrcalc374");
    BOOST_CHECK(fct.equalTo("inp5s.map",false));
    // debug map not written
    BOOST_CHECK(!com::exists("failureExpected.map"));
  }
  {
    // pcrcalc 375
    // defined twice in external binding is ok
    geo::FileCreateTester fct("tmp.res");
    com::write("Mul=0;Mul = 5;",pn);
    execTest("pcrcalc374");
    BOOST_CHECK(fct.equalTo("inp5s.map",false));
  }
}

BOOST_AUTO_TEST_CASE(testTimeStepHack)
{
  using namespace calc;

    { // check what is written
      // timer 3 6 1;
      TestAsciiResult res;
      execTest("pcrcalc528");
      // all stack
      BOOST_CHECK( com::exists("tmp52800.006"));
      BOOST_CHECK( com::exists("tmp52800.005"));
      BOOST_CHECK( com::exists("tmp52800.004"));
      BOOST_CHECK( com::exists("tmp52800.003"));
      BOOST_CHECK(!com::exists("tmp52800.002"));
      BOOST_CHECK(!com::exists("tmp52800.001"));
      // endTime
      BOOST_CHECK( com::exists("tmpL5280.006"));
      BOOST_CHECK(!com::exists("tmpL5280.005"));
      BOOST_CHECK(!com::exists("tmpL5280.004"));
      BOOST_CHECK(!com::exists("tmpL5280.003"));
      BOOST_CHECK(!com::exists("tmpL5280.002"));
      BOOST_CHECK(!com::exists("tmpL5280.001"));
      // 4,5
      BOOST_CHECK(!com::exists("tmp45528.006"));
      BOOST_CHECK( com::exists("tmp45528.005"));
      BOOST_CHECK( com::exists("tmp45528.004"));
      BOOST_CHECK(!com::exists("tmp45528.003"));
      BOOST_CHECK(!com::exists("tmp45528.002"));
      BOOST_CHECK(!com::exists("tmp45528.001"));
      // tss:
      BOOST_CHECK(res.equals("pcrcalc528"));
    }
    { // check only for availability of part used
      com::copy("inp1s.map", "tmp52900.003");
      com::copy("inp5s.map", "tmp52900.004");
      com::copy("inp1s.map", "tmp52900.005");

      TestAsciiResult res;
      execTest("pcrcalc529");
      BOOST_CHECK(res.equals("pcrcalc529"));
    }
    {
       TestAsciiResult res;
       execTest("pcrcalc530");
       BOOST_CHECK(res.equals("pcrcalc530"));
    }
    {
       TestAsciiResult res;
       execTest("pcrcalc531");
       // TO LONG BOOST_CHECK(res.equals("pcrcalc531"));
    }
}

BOOST_AUTO_TEST_CASE(testLinkInLibrary)
{
  using namespace calc;

    {
       TestAsciiResult res;
       try {
       execTest("pcrcalc526");
       } catch(com::Exception const& e) {
         BOOST_CHECK(e.messages().find( "noCodeLinkIn") != std::string::npos);
#ifdef WIN32
         BOOST_CHECK(e.messages().find( "The specified module could not be found.") != std::string::npos);
#elif __APPLE__
         BOOST_CHECK(e.messages().find( "image not found") != std::string::npos);
#else
         BOOST_CHECK(e.messages().find( "cannot open shared object file") != std::string::npos);
#endif
       }
    }
    {
       TestAsciiResult res;
       try {
       execTest("pcrcalc551");
       } catch(com::Exception const& e) {
         // Message: unable to open primary document entity '*/noXMLLinkIn'
         BOOST_CHECK(e.messages().find( "noXMLLinkIn") != std::string::npos);
       }
    }
    {
       TestAsciiResult res;
       execTest("pcrcalc552");
    }
    {
       TestAsciiResult res;
       execTest("pcrcalc553");
    }
    {
      geo::FileCreateTester fct("tmp.res");
      execTest("pcrcalc554");
      BOOST_CHECK(fct.equalTo("inp5s.map",false));
    }
    {
      geo::FileCreateTester fct("tmp556.res");
      execTest("pcrcalc556");
      BOOST_CHECK(fct.equalTo("inp5s.map",false));
    }
}
