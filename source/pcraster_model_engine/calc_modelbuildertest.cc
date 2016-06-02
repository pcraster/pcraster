#define BOOST_TEST_MODULE pcraster model_engine modelbuilder
#include <boost/test/unit_test.hpp>
#include "geo_filecreatetester.h"
#include "com_pathinfo.h"
#include "com_exception.h"
#include "com_interval.h"
#include "com_file.h"
#include "calc_modelbuilder.h"
#include "calc_posexception.h"
#include "calc_lookuptable.h"


// OLS 05/2016 these unit tests were not active in the old setup


// // // BOOST_AUTO_TEST_CASE(testSingleStatement)
// // // {
// // //   using namespace calc;
// // //
// // //   geo::FileCreateTester mt("ModelBuildertestSS.res");
// // //   ModelBuilder mb;
// // //   mb.addStatement("ModelBuildertestSS.res = inp1s.map + 4;");
// // //   mb.execute();
// // //   BOOST_CHECK(mt.equalTo("inp5s.map",false));
// // // }


// // // BOOST_AUTO_TEST_CASE(testASTExpr)
// // // {
// // //   using namespace calc;
// // //
// // //   try {
// // //   geo::FileCreateTester mt("ModelBuildertestFE.res");
// // //   ModelBuilder mb;
// // //   ASTExpr* e= mb.addASTExpr("inp1s.map + 4;");
// // //   mb.addFieldAssignment("ModelBuildertestFE.res",e,true);
// // //   mb.execute();
// // //   BOOST_CHECK(mt.equalTo("inp5s.map",false));
// // //   } catch (const com::Exception& e) {
// // //     std::cerr << e.messages() << "\n";
// // //   } catch (const std::exception& e) {
// // //     std::cerr << e.what() << "\n";
// // //   }
// // // }

// // // BOOST_AUTO_TEST_CASE(testMultipleStatements)
// // // {
// // //   using namespace calc;
// // //
// // //   try {
// // //   geo::FileCreateTester mt("ModelBuildertestMS.res");
// // //   ModelBuilder mb;
// // //   // test order by dependency, and selective report!
// // //   BOOST_CHECK(!com::pathExists("ModelBuildertestMS4.res"));
// // //   // do not write this one
// // //   mb.addStatement("ModelBuildertestMS4.res = 3+inp1s.map;",false);
// // //   mb.addStatement("ModelBuildertestMS.res = ModelBuildertestMS4.res + 1;");
// // //   mb.execute();
// // //   BOOST_CHECK(mt.equalTo("inp5s.map",false));
// // //   BOOST_CHECK(!com::pathExists("ModelBuildertestMS4.res"));
// // //   } catch (...) {
// // //     BOOST_CHECK(false);
// // //   }
// // // }

// // // BOOST_AUTO_TEST_CASE(testMultipleStatementsWithBinding)
// // // {
// // //   using namespace calc;
// // //
// // //   try {
// // //   geo::FileCreateTester mt("ModelBuilderBind5.res");
// // //   ModelBuilder mb;
// // //   mb.addBinding("Inp1s","inp1s.map");
// // //   mb.addBinding("Result","ModelBuilderBind5.res");
// // //   mb.evaluateBindings();
// // //   mb.addStatement("ModelBuilderBind.res = 3+Inp1s;");
// // //   mb.addStatement("Result= ModelBuilderBind.res + 1;");
// // //   mb.execute();
// // //   BOOST_CHECK( com::pathExists("ModelBuilderBind.res"));
// // //   BOOST_CHECK( com::pathExists("ModelBuilderBind5.res"));
// // //   BOOST_CHECK(mt.equalTo("inp5s.map",false));
// // //   } catch (...) {
// // //     BOOST_CHECK(false);
// // //   }
// // // }


// // // BOOST_AUTO_TEST_CASE(testMultipleStatementsWithError)
// // // {
// // //   using namespace calc;
// // //
// // //   bool failure=false;
// // //   try {
// // //   geo::FileCreateTester mt("ModelBuildertestMS.res");
// // //   ModelBuilder mb;
// // //   // test order by dependency, and selective report!
// // //   mb.addStatement("ModelBuildertestMS4.res = 3+1");
// // //   mb.addStatement("ModelBuildertestMS.res = failureExpectedNotExistant + 1");
// // //   mb.execute();
// // //   BOOST_CHECK( com::pathExists("ModelBuildertestMS4.res"));
// // //   BOOST_CHECK(mt.equalTo("inp5s.map",false));
// // //   } catch (const calc::PosException& e) {
// // //     failure=true;
// // //     BOOST_CHECK(e.messages().find("failureExpectedNotExistant")
// // //               != std::string::npos);
// // //   }
// // //   BOOST_CHECK(failure);
// // // }

// // // BOOST_AUTO_TEST_CASE(testSetValuescale)
// // // {
// // //   using namespace calc;
// // //
// // //     { // avoid this for habitat
// // //       bool failure=false;
// // //       try {
// // //       ModelBuilder mb;
// // //       mb.addStatement("ModelBuilderVsUnknown.res = if (inp1b.map, 1);");
// // //       mb.execute();
// // //       } catch (const calc::PosException& e) {
// // //         BOOST_CHECK(e.messages().find("ModelBuilderVsUnknown.res")
// // //                   != std::string::npos);
// // //         BOOST_CHECK(e.messages().find("conversion")
// // //                   != std::string::npos);
// // //         failure=true;
// // //       }
// // //       BOOST_CHECK(failure);
// // //   }
// // //   { // solve simple
// // //     geo::FileCreateTester mt("ModelBuilderVsUnknown.res");
// // //     ModelBuilder mb;
// // //     mb.addStatement("ModelBuilderVsUnknown.res = if (inp1b.map, scalar(1));");
// // //     mb.execute();
// // //     BOOST_CHECK( com::pathExists("ModelBuilderVsUnknown.res"));
// // //     BOOST_CHECK(mt.equalTo("inp1s.map",false));
// // //   }
// // //   { // TODO  solve by setting
// // //     try {
// // //     geo::FileCreateTester mt("ModelBuilderVsUnknown.res");
// // //     ModelBuilder mb;
// // //     // anders
// // //     mb.addStatement("ModelBuilderVsUnknown.res = scalar(1);");
// // //     mb.execute();
// // //     BOOST_CHECK( com::pathExists("ModelBuilderVsok.res"));
// // //     BOOST_CHECK( com::pathExists("ModelBuilderVsUnknown.res"));
// // //     BOOST_CHECK(mt.equalTo("inp1s.map",false));
// // //     } catch (...) {
// // //       ; // BOOST_CHECK(false);
// // //     }
// // //   }
// // // }

// // // BOOST_AUTO_TEST_CASE(testAddLookupTable)
// // // {
// // //   using namespace calc;
// // //
// // //   geo::FileCreateTester mt("ModelBuildertestAddLT.res");
// // //   ModelBuilder mb;
// // //
// // //   LookupTable::Records lr;
// // //   LookupRecord::Key key;
// // //   key.push_back(new com::EqualTo<double>(1));
// // //   lr.push_back(LookupRecord(key,5));
// // //   delete key[0];
// // //   LookupTable *tab = new LookupTable();
// // //   std::vector<VS> colVs(2);
// // //   colVs[0] = VS_N;
// // //   colVs[1] = VS_S;
// // //   tab->setRecords(lr,colVs);
// // //
// // //   mb.addLookupTable("onlyInRamTable",tab);
// // //   std::ostringstream expr;
// // //
// // //   ASTExpr* e= mb.addASTExpr("lookupscalar(onlyInRamTable,inp1n.map)");
// // //   mb.addFieldAssignment("ModelBuildertestAddLT.res",e,true);
// // //   mb.execute();
// // //
// // //   BOOST_CHECK(mt.equalTo("inp5s.map",false));
// // //
// // // }
