#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_MODELBUILDERTEST
#include "calc_modelbuildertest.h"
#define INCLUDED_CALC_MODELBUILDERTEST
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
#ifndef INCLUDED_GEO_FILECREATETESTER
#include "geo_filecreatetester.h"
#define INCLUDED_GEO_FILECREATETESTER
#endif
#ifndef INCLUDED_COM_PATHINFO
#include "com_pathinfo.h"
#define INCLUDED_COM_PATHINFO
#endif
#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif
#ifndef INCLUDED_COM_INTERVAL
#include "com_interval.h"
#define INCLUDED_COM_INTERVAL
#endif
#ifndef INCLUDED_COM_FILE
#include "com_file.h"
#define INCLUDED_COM_FILE
#endif
// Module headers.
#ifndef INCLUDED_CALC_MODELBUILDER
#include "calc_modelbuilder.h"
#define INCLUDED_CALC_MODELBUILDER
#endif

#ifndef INCLUDED_CALC_POSEXCEPTION
#include "calc_posexception.h"
#define INCLUDED_CALC_POSEXCEPTION
#endif
#ifndef INCLUDED_CALC_LOOKUPTABLE
#include "calc_lookuptable.h"
#define INCLUDED_CALC_LOOKUPTABLE
#endif

/*!
  \file
  This file contains the implementation of the ModelBuilderTest class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC MODELBUILDER MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*calc::ModelBuilderTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<ModelBuilderTest> instance(new ModelBuilderTest());

  suite->add(BOOST_CLASS_TEST_CASE(&ModelBuilderTest::testSingleStatement, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&ModelBuilderTest::testASTExpr, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&ModelBuilderTest::testMultipleStatements, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&ModelBuilderTest::testMultipleStatementsWithError, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&ModelBuilderTest::testMultipleStatementsWithBinding, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&ModelBuilderTest::testSetValuescale, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&ModelBuilderTest::testAddLookupTable, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&ModelBuilderTest::testExternalBindings, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF MODELBUILDER MEMBERS
//------------------------------------------------------------------------------

//! ctor
calc::ModelBuilderTest::ModelBuilderTest()
{
}



//! setUp
void calc::ModelBuilderTest::setUp()
{
}

//! tearDown
void calc::ModelBuilderTest::tearDown()
{
}



void calc::ModelBuilderTest::testSingleStatement()
{
  geo::FileCreateTester mt("ModelBuildertestSS.res");
  ModelBuilder mb;
  mb.addStatement("ModelBuildertestSS.res = inp1s.map + 4;");
  mb.execute();
  BOOST_CHECK(mt.equalTo("inp5s.map",false));
}

void calc::ModelBuilderTest::testASTExpr()
{
  try {
  geo::FileCreateTester mt("ModelBuildertestFE.res");
  ModelBuilder mb;
  ASTExpr* e= mb.addASTExpr("inp1s.map + 4;");
  mb.addFieldAssignment("ModelBuildertestFE.res",e,true);
  mb.execute();
  BOOST_CHECK(mt.equalTo("inp5s.map",false));
  } catch (const com::Exception& e) {
    std::cerr << e.messages() << "\n";
  } catch (const std::exception& e) {
    std::cerr << e.what() << "\n";
  }
}

void calc::ModelBuilderTest::testMultipleStatements()
{
  try {
  geo::FileCreateTester mt("ModelBuildertestMS.res");
  ModelBuilder mb;
  // test order by dependency, and selective report!
  BOOST_CHECK(!com::pathExists("ModelBuildertestMS4.res"));
  // do not write this one
  mb.addStatement("ModelBuildertestMS4.res = 3+inp1s.map;",false);
  mb.addStatement("ModelBuildertestMS.res = ModelBuildertestMS4.res + 1;");
  mb.execute();
  BOOST_CHECK(mt.equalTo("inp5s.map",false));
  BOOST_CHECK(!com::pathExists("ModelBuildertestMS4.res"));
  } catch (...) {
    BOOST_CHECK(false);
  }
}

void calc::ModelBuilderTest::testMultipleStatementsWithBinding()
{
  try {
  geo::FileCreateTester mt("ModelBuilderBind5.res");
  ModelBuilder mb;
  mb.addBinding("Inp1s","inp1s.map");
  mb.addBinding("Result","ModelBuilderBind5.res");
  mb.evaluateBindings();
  mb.addStatement("ModelBuilderBind.res = 3+Inp1s;");
  mb.addStatement("Result= ModelBuilderBind.res + 1;");
  mb.execute();
  BOOST_CHECK( com::pathExists("ModelBuilderBind.res"));
  BOOST_CHECK( com::pathExists("ModelBuilderBind5.res"));
  BOOST_CHECK(mt.equalTo("inp5s.map",false));
  } catch (...) {
    BOOST_CHECK(false);
  }
}


void calc::ModelBuilderTest::testMultipleStatementsWithError()
{
  bool failure=false;
  try {
  geo::FileCreateTester mt("ModelBuildertestMS.res");
  ModelBuilder mb;
  // test order by dependency, and selective report!
  mb.addStatement("ModelBuildertestMS4.res = 3+1");
  mb.addStatement("ModelBuildertestMS.res = failureExpectedNotExistant + 1");
  mb.execute();
  BOOST_CHECK( com::pathExists("ModelBuildertestMS4.res"));
  BOOST_CHECK(mt.equalTo("inp5s.map",false));
  } catch (const calc::PosException& e) {
    failure=true;
    BOOST_CHECK(e.messages().find("failureExpectedNotExistant")
              != std::string::npos);
  }
  BOOST_CHECK(failure);
}

void calc::ModelBuilderTest::testSetValuescale()
{
    { // avoid this for habitat
      bool failure=false;
      try {
      ModelBuilder mb;
      mb.addStatement("ModelBuilderVsUnknown.res = if (inp1b.map, 1);");
      mb.execute();
      } catch (const calc::PosException& e) {
        BOOST_CHECK(e.messages().find("ModelBuilderVsUnknown.res")
                  != std::string::npos);
        BOOST_CHECK(e.messages().find("conversion")
                  != std::string::npos);
        failure=true;
      }
      BOOST_CHECK(failure);
  }
  { // solve simple
    geo::FileCreateTester mt("ModelBuilderVsUnknown.res");
    ModelBuilder mb;
    mb.addStatement("ModelBuilderVsUnknown.res = if (inp1b.map, scalar(1));");
    mb.execute();
    BOOST_CHECK( com::pathExists("ModelBuilderVsUnknown.res"));
    BOOST_CHECK(mt.equalTo("inp1s.map",false));
  }
  { // TODO  solve by setting
    try {
    geo::FileCreateTester mt("ModelBuilderVsUnknown.res");
    ModelBuilder mb;
    // anders
    mb.addStatement("ModelBuilderVsUnknown.res = scalar(1);");
    mb.execute();
    BOOST_CHECK( com::pathExists("ModelBuilderVsok.res"));
    BOOST_CHECK( com::pathExists("ModelBuilderVsUnknown.res"));
    BOOST_CHECK(mt.equalTo("inp1s.map",false));
    } catch (...) {
      ; // BOOST_CHECK(false);
    }
  }
}

void calc::ModelBuilderTest::testAddLookupTable()
{
  geo::FileCreateTester mt("ModelBuildertestAddLT.res");
  ModelBuilder mb;

  LookupTable::Records lr;
  LookupRecord::Key key;
  key.push_back(new com::EqualTo<double>(1));
  lr.push_back(LookupRecord(key,5));
  delete key[0];
  LookupTable *tab = new LookupTable();
  std::vector<VS> colVs(2);
  colVs[0] = VS_N;
  colVs[1] = VS_S;
  tab->setRecords(lr,colVs);

  mb.addLookupTable("onlyInRamTable",tab);
  std::ostringstream expr;

  ASTExpr* e= mb.addASTExpr("lookupscalar(onlyInRamTable,inp1n.map)");
  mb.addFieldAssignment("ModelBuildertestAddLT.res",e,true);
  mb.execute();

  BOOST_CHECK(mt.equalTo("inp5s.map",false));

}
