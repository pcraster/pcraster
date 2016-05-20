#error

// OLS: kept this file for documentation purposes
//      until unit tests work properly again on all platforms




#define BOOST_TEST_MODULE calc test suite
#include "stddefx.h"



#ifndef INCLUDED_BOOST_TEST_INCLUDED_UNIT_TEST
#include <boost/test/included/unit_test.hpp>
#define INCLUDED_BOOST_TEST_INCLUDED_UNIT_TEST
#endif
#ifndef INCLUDED_CALC_LIBRARYCLASS
#include "calc_LibraryClass.h"
#define INCLUDED_CALC_LIBRARYCLASS
#endif
/*
#ifndef INCLUDED_CALC_IOBANDFIELDSTRATEGYTEST
#include "calc_iobandfieldstrategytest.h"
#define INCLUDED_CALC_IOBANDFIELDSTRATEGYTEST
#endif
#ifndef INCLUDED_CALC_IOBANDTEST
#include "calc_iobandtest.h"
#define INCLUDED_CALC_IOBANDTEST
#endif
*/
#ifndef INCLUDED_CALC_SCRIPTTEST
#include "calc_scripttest.h"
#define INCLUDED_CALC_SCRIPTTEST
#endif
#ifndef INCLUDED_CALC_CLIENTINTERFACETEST
#include "calc_clientinterfacetest.h"
#define INCLUDED_CALC_CLIENTINTERFACETEST
#endif
#ifndef INCLUDED_CALC_OBJECTLINKMETATEST
#include "calc_objectlinkmetatest.h"
#define INCLUDED_CALC_OBJECTLINKMETATEST
#endif
#ifndef INCLUDED_CALC_OBJECTLINKTEST
#include "calc_objectlinktest.h"
#define INCLUDED_CALC_OBJECTLINKTEST
#endif
#ifndef INCLUDED_CALC_LDDGRAPHTEST
#include "calc_lddgraphtest.h"
#define INCLUDED_CALC_LDDGRAPHTEST
#endif
#ifndef INCLUDED_CALC_POINTCODEBLOCKDLLTEST
#include "calc_pointcodeblockdlltest.h"
#define INCLUDED_CALC_POINTCODEBLOCKDLLTEST
#endif

#ifndef INCLUDED_CALC_POINTCODEBLOCKREPLACERTEST
#include "calc_pointcodeblockreplacertest.h"
#define INCLUDED_CALC_POINTCODEBLOCKREPLACERTEST
#endif
#ifndef INCLUDED_CALC_USEDEFANALYZERTEST
#include "calc_usedefanalyzertest.h"
#define INCLUDED_CALC_USEDEFANALYZERTEST
#endif
#ifndef INCLUDED_CALC_RUNDIRECTORYTEST
#include "calc_rundirectorytest.h"
#define INCLUDED_CALC_RUNDIRECTORYTEST
#endif
#ifndef INCLUDED_CALC_GLOBALLIBDEFS
#include "calc_globallibdefs.h" // globalInit
#define INCLUDED_CALC_GLOBALLIBDEFS
#endif
#ifndef INCLUDED_CALC_IOCSFFIELDSTRATEGYTEST
#include "calc_iocsffieldstrategytest.h"
#define INCLUDED_CALC_IOCSFFIELDSTRATEGYTEST
#endif

#ifndef INCLUDED_CALC_MANUALEXAMPLETESTERTEST
#include "calc_manualexampletestertest.h"
#define INCLUDED_CALC_MANUALEXAMPLETESTERTEST
#endif
#ifndef INCLUDED_CALC_MODELBUILDERTEST
#include "calc_modelbuildertest.h"
#define INCLUDED_CALC_MODELBUILDERTEST
#endif
#ifndef INCLUDED_CALC_MANUALEXAMPLESTEST
#include "calc_manualexamplestest.h"
#define INCLUDED_CALC_MANUALEXAMPLESTEST
#endif
#ifndef INCLUDED_CALC_TSSOUTPUTVALUETEST
#include "calc_tssoutputvaluetest.h"
#define INCLUDED_CALC_TSSOUTPUTVALUETEST
#endif

#ifndef INCLUDED_CALC_LOOKUPTABLETEST
#include "calc_lookuptabletest.h"
#define INCLUDED_CALC_LOOKUPTABLETEST
#endif

#ifndef INCLUDED_CALC_NONSPATIALTEST
#include "calc_nonspatialtest.h"
#define INCLUDED_CALC_NONSPATIALTEST
#endif
#ifndef INCLUDED_CALC_SPATIALTEST
#include "calc_spatialtest.h"
#define INCLUDED_CALC_SPATIALTEST
#endif

#ifndef INCLUDED_CALC_MASKPACKINGTEST
#include "calc_maskpackingtest.h"
#define INCLUDED_CALC_MASKPACKINGTEST
#endif
#ifndef INCLUDED_CALC_DATATYPETEST
#include "calc_datatypetest.h"
#define INCLUDED_CALC_DATATYPETEST
#endif
#ifndef INCLUDED_CALC_BUILDTYPESVISITORTEST
#include "calc_buildtypesvisitortest.h"
#define INCLUDED_CALC_BUILDTYPESVISITORTEST
#endif
#ifndef INCLUDED_CALC_FINDSYMBOLTEST
#include "calc_findsymboltest.h"
#define INCLUDED_CALC_FINDSYMBOLTEST
#endif
#ifndef INCLUDED_CALC_OPERATORTEST
#include "calc_operatortest.h"
#define INCLUDED_CALC_OPERATORTEST
#endif
#ifndef INCLUDED_CALC_ASTSYMBOLTABLETEST
#include "calc_astsymboltabletest.h"
#define INCLUDED_CALC_ASTSYMBOLTABLETEST
#endif
#ifndef INCLUDED_CALC_LEXINPUTTEST
#include "calc_lexinputtest.h"
#define INCLUDED_CALC_LEXINPUTTEST
#endif
#ifndef INCLUDED_CALC_PARSERTEST
#include "calc_parsertest.h"
#define INCLUDED_CALC_PARSERTEST
#endif
#ifndef INCLUDED_CALC_ASTPARTEST
#include "calc_astpartest.h"
#define INCLUDED_CALC_ASTPARTEST
#endif
#ifndef INCLUDED_CALC_REPORTTEST
#include "calc_reporttest.h"
#define INCLUDED_CALC_REPORTTEST
#endif
#ifndef INCLUDED_CALC_CFGCREATORTEST
#include "calc_cfgcreatortest.h"
#define INCLUDED_CALC_CFGCREATORTEST
#endif

#ifndef INCLUDED_CALC_FOPOINTARRAYTEST
#include "calc_fopointarraytest.h"
#define INCLUDED_CALC_FOPOINTARRAYTEST
#endif
#ifndef INCLUDED_CALC_FIELDTEST
#include "calc_fieldtest.h"
#define INCLUDED_CALC_FIELDTEST
#endif
#ifndef INCLUDED_CALC_VFIELDTEST
#include "calc_vfieldtest.h"
#define INCLUDED_CALC_VFIELDTEST
#endif
#ifndef INCLUDED_CALC_OPIMPLTEST
#include "calc_opimpltest.h"
#define INCLUDED_CALC_OPIMPLTEST
#endif
#ifndef INCLUDED_CALC_REPORTVISITORTEST
#include "calc_reportvisitortest.h"
#define INCLUDED_CALC_REPORTVISITORTEST
#endif
#ifndef INCLUDED_CALC_DYNAMICWAVETEST
#include "calc_dynamicwavetest.h"
#define INCLUDED_CALC_DYNAMICWAVETEST
#endif
#ifndef INCLUDED_CALC_EXECUTORTEST
#include "calc_executortest.h"
#define INCLUDED_CALC_EXECUTORTEST
#endif
#ifndef INCLUDED_CALC_BINDINGTABLETEST
#include "calc_bindingtabletest.h"
#define INCLUDED_CALC_BINDINGTABLETEST
#endif
#ifndef INCLUDED_CALC_RUNTIMEENVTEST
#include "calc_runtimeenvtest.h"
#define INCLUDED_CALC_RUNTIMEENVTEST
#endif
#ifndef INCLUDED_CALC_ASTPATHTEST
#include "calc_astpathtest.h"
#define INCLUDED_CALC_ASTPATHTEST
#endif
#ifndef INCLUDED_CALC_CMDLINECALCTEST
#include "calc_cmdlinecalctest.h"
#define INCLUDED_CALC_CMDLINECALCTEST
#endif
#ifndef INCLUDED_CALC_AVERAGEMAPTEST
#include "calc_averagemaptest.h"
#define INCLUDED_CALC_AVERAGEMAPTEST
#endif
#ifndef INCLUDED_CALC_AREAOPERATIONSTEST
#include "calc_areaoperationstest.h"
#define INCLUDED_CALC_AREAOPERATIONSTEST
#endif
#ifndef INCLUDED_CALC_ARGORDERTEST
#include "calc_argordertest.h"
#define INCLUDED_CALC_ARGORDERTEST
#endif
#ifndef INCLUDED_CALC_DIMENSIONPARSERTEST
#include "calc_dimensionparsertest.h"
#define INCLUDED_CALC_DIMENSIONPARSERTEST
#endif
#ifndef INCLUDED_CALC_RUNTIMEENGINETEST
#include "calc_runtimeenginetest.h"
#define INCLUDED_CALC_RUNTIMEENGINETEST
#endif
#ifndef INCLUDED_CALC_XMLREFLECTIONTEST
#include "calc_xmlreflectiontest.h"
#define INCLUDED_CALC_XMLREFLECTIONTEST
#endif
#ifndef INCLUDED_CALC_AREAMAPTEST
#include "calc_areamaptest.h"
#define INCLUDED_CALC_AREAMAPTEST
#endif
#ifndef INCLUDED_CALC_IOCSFFIELDSTRATEGYTEST
#include "calc_iocsffieldstrategytest.h"
#define INCLUDED_CALC_IOCSFFIELDSTRATEGYTEST
#endif

extern "C" int appUnitTest; // app/appinst.c

 boost::unit_test::test_suite* init_unit_test_suite(int argc, char ** const argv) {
  calc::globalInit();
  appUnitTest=1;

  struct TestSuite: public boost::unit_test::test_suite,
                    public calc::LibraryClassNoQt
  {
    TestSuite(
         int /* argc */,
         char** argv)
      : boost::unit_test::test_suite("Master test suite"),
        calc::LibraryClassNoQt(argv[0])
    {
      assert(dev::GDalClient::isInitialized());
      assert(dal::Client::isInitialized());
    }

    ~TestSuite()
    {
      // TODO globalEnd resets a static pointer to a ClientHolder
      //      instance. This instance is a LibraryClassNoQt instance. It seems
      //      that when this instance is destructed too late, the gdal lib is
      //      already not available anymore. Maybe because of some static state
      //      in that library. We must make sure that our static stuff is reset
      //      before we finish the tests.
      //      Get rid of the static ClientHolder instance, in favor of
      //      some non-static singleton.
      calc::globalEnd();
    }

  };

  TestSuite* test = new TestSuite(argc, argv);

  test->add(calc::AreaMapTest().suite());
  test->add(calc::ObjectLinkMetaTest().suite());
  test->add(calc::ObjectLinkTest().suite());
  test->add(calc::OperatorTest().suite());
  test->add(calc::OpImplTest().suite());
  test->add(calc::FoPointArrayTest().suite());
  test->add(calc::FindSymbolTest().suite());
  test->add(calc::ASTParTest().suite());

  test->add(calc::AverageMapTest::suite());
  test->add(calc::AreaOperationsTest::suite());
  test->add(calc::ArgOrderTest::suite());
  test->add(calc::LddGraphTest::suite());
  test->add(calc::DimensionParserTest().suite());
  test->add(calc::NonSpatialTest().suite());
  test->add(calc::SpatialTest::suite());
  test->add(calc::FieldTest().suite());
  test->add(calc::VFieldTest().suite());
  test->add(calc::LookupTableTest().suite());

  test->add(calc::UseDefAnalyzerTest().suite());
  test->add(calc::LexInputTest().suite());
  test->add(calc::ParserTest().suite());
  test->add(calc::CFGCreatorTest().suite());
  test->add(calc::ASTPathTest().suite());
  test->add(calc::DataTypeTest().suite());
  test->add(calc::BuildTypesVisitorTest().suite());
  test->add(calc::ASTSymbolTableTest().suite());
  test->add(calc::ReportTest().suite());

  test->add(calc::ReportVisitorTest().suite());

  test->add(calc::BindingTableTest().suite());
  test->add(calc::TssOutputValueTest().suite());
  test->add(calc::MaskPackingTest::suite());
  test->add(calc::RunDirectoryTest().suite());
  test->add(calc::RunTimeEnvTest().suite());

  test->add(calc::DynamicWaveTest().suite());
  test->add(calc::IoFieldStrategyTest().suite());
  test->add(calc::ClientInterfaceTest().suite());

  test->add(calc::APIInitTest().suite());
  test->add(calc::XMLReflectionTest().suite());
  test->add(calc::ExecutorTest().suite());
  test->add(calc::CmdLineCalcTest().suite());

  test->add(calc::ManualExampleTesterTest().suite());
  test->add(calc::ManualExamplesTest().suite());

  test->add(calc::PointCodeBlockReplacerTest().suite());
  test->add(calc::PointCodeBlockDllTest().suite());

  test->add(calc::RunTimeEngineTest().suite());

  test->add(calc::ScriptTest().suite());

  /* OLD STUFF NO NEED FOR TESTING
  * test->add(calc::ModelBuilderTest().suite());
  * test->add(calc::IoBandFieldStrategyTest().suite());
  * // must be last, since global option appIoStrategy is changed
  * test->add(calc::IoBandTest().suite());
  */

  return test;
}

