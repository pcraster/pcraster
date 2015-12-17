#define BOOST_TEST_MODULE pcraster old_calc model_builder
#include <boost/test/unit_test.hpp>
#include "com_algorithm.h"
#include "geo_filecreatetester.h"
#include "com_pathinfo.h"
#include "com_exception.h"
#include "com_interval.h"
#include "com_file.h"
#include "calc_modelbuilder.h"
#include "calc_posexception.h"
#include "calc_lookuptable.h"


BOOST_AUTO_TEST_CASE(single_statement)
{
  using namespace calc;

  ModelBuilder mb;
  geo::FileCreateTester mt("ModelBuildertestSS.res");
  mb.addStatement("ModelBuildertestSS.res = inp1s.map + 4;");
  mb.execute();
  BOOST_CHECK(mt.equalTo("inp5s.map",false));
}


BOOST_AUTO_TEST_CASE(field_expr)
{
  using namespace calc;

  try {
  ModelBuilder mb;
  FieldExpr* e= mb.addFieldExpr("inp1s.map + 4;");
  geo::FileCreateTester mt("ModelBuildertestFE.res");
  mb.addFieldAssignment("ModelBuildertestFE.res",e,true);
  mb.execute();
  BOOST_CHECK(mt.equalTo("inp5s.map",false));
  } catch (const com::Exception& e) {
    std::cerr << e.messages() << "\n";
  } catch (const std::exception& e) {
    std::cerr << e.what() << "\n";
  }
}


BOOST_AUTO_TEST_CASE(multiple_statements)
{
  using namespace calc;

  try {
  ModelBuilder mb;
  // test order by dependency, and selective report!
  BOOST_CHECK(!com::pathExists("ModelBuildertestMS4.res"));
  geo::FileCreateTester mt("ModelBuildertestMS.res");
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


BOOST_AUTO_TEST_CASE(multiple_statements_with_binding)
{
  using namespace calc;

  try {
  ModelBuilder mb;
  geo::FileCreateTester mt("ModelBuilderBind5.res");
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


BOOST_AUTO_TEST_CASE(multiple_statements_with_error)
{
  using namespace calc;

  bool failure=false;
  try {
  ModelBuilder mb;
  geo::FileCreateTester mt("ModelBuildertestMS.res");
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


BOOST_AUTO_TEST_CASE(set_value_scale)
{
  using namespace calc;

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
    ModelBuilder mb;
    geo::FileCreateTester mt("ModelBuilderVsUnknown.res");
    mb.addStatement("ModelBuilderVsUnknown.res = if (inp1b.map, scalar(1));");
    mb.execute();
    BOOST_CHECK( com::pathExists("ModelBuilderVsUnknown.res"));
    BOOST_CHECK(mt.equalTo("inp1s.map",false));
  }
  { // TODO  solve by setting
    try {
    ModelBuilder mb;
    geo::FileCreateTester mt("ModelBuilderVsUnknown.res");
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


BOOST_AUTO_TEST_CASE(add_lookup_table)
{
  using namespace calc;

  ModelBuilder mb;

  LookupTable::Records lr;
  LookupRecord::Key key;
  key.push_back(new com::EqualTo<double>(1));
  lr.push_back(LookupRecord(key,5));
  delete key[0];
  LookupTable *tab = new LookupTable(VS_S);
  std::vector<VS> readKeys(1);
  readKeys[0] = VS_N;
  tab->setRecords(lr,readKeys);

  mb.addLookupTable("onlyInRamTable",tab);
  std::ostringstream expr;

  geo::FileCreateTester mt("ModelBuildertestAddLT.res");

  FieldExpr* e= mb.addFieldExpr("lookupscalar(onlyInRamTable,inp1n.map)");
  mb.addFieldAssignment("ModelBuildertestAddLT.res",e,true);
  mb.execute();

  BOOST_CHECK(mt.equalTo("inp5s.map",false));

}


BOOST_AUTO_TEST_CASE(external_bindings)
{
  using namespace calc;

 // SYNTAX ERROR
 bool catched(false);
 try {
    ModelBuilder mb;
    com::PathName pn("testAddBindings.txt");
    com::write("jan=3; #comment\njan=cees + 4\n",
                "testAddBindings.txt");
    mb.parseExternalBindings(pn);
 } catch (calc::PosException e) {
  catched=true;
 }
 BOOST_CHECK(catched);

 // CORRECT
 try {
    ModelBuilder mb;
    com::PathName pn("testAddBindings.txt");
    com::write("jan=3.5; #comment\njan=\"xx file.txt\"\nn=4",
                "testAddBindings.txt");
    RunSettings rs=mb.parseExternalBindings(pn);

    // jan = 3.5 overwritten by jan = xx file.txt
    ExtSym  none("xx");
    com::FindValue<ExtSym,ExtSym> fv(none);
    BOOST_CHECK(fv.find(rs.bindings(),ExtSym("jan")).name() == "xx file.txt");

    BOOST_CHECK(fv.find(rs.bindings(),ExtSym("n")).name() == "4");

 } catch (...) {
  BOOST_CHECK(false);
 }
}
