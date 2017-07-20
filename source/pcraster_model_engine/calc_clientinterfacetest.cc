#define BOOST_TEST_MODULE pcraster model_engine clientinterface
#include <boost/test/unit_test.hpp>
#include "stddefx.h"
#include <algorithm>
#include <boost/filesystem/path.hpp>
#include <boost/filesystem/operations.hpp>
namespace fs=boost::filesystem;

#include "appargs.h"
#include "calc_comparefilewithvalidated.h"
#include "calc_stattable.h"
#include "geo_filecreatetester.h"
#include "com_file.h"
#include "calc_globallibdefs.h"
#include "pcrcalc.h"
#include "calc_asttestfactory.h"
#include "calc_astscript.h"
#include "calc_astsymbolinfo.h"
#include "calc_xmlreflection.h"
#include "calc_spatial.h"


namespace calc {
   namespace detail {
    static bool validateBil(std::string const& name)
    {
      static const char *exts[3] = { ".bil",".hdr",".stx"};
      for(size_t i =0; i < 3; ++i)
        if (!compareFileWithValidated(name+exts[i]))
          return false;
      return true;
    }
    static bool pcr_ScriptErrorMessage_contains(
     const char* returned,
     const char*expected)
    {
       return std::string(returned).find( expected)!= std::string::npos;
    }

#define BOOST_CHECK_MESSAGE_ErrorMessage(s, expected) { \
     const char *msg = pcr_ScriptErrorMessage(s); \
     BOOST_CHECK_MESSAGE(calc::detail::pcr_ScriptErrorMessage_contains(msg,expected),msg); }

    //! \todo duplicated test code
    struct FakeDevLicense {
      FakeDevLicense()
      {
       globalInit();
      }
      ~FakeDevLicense()
      {
      }
    };

  }
}

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

BOOST_AUTO_TEST_CASE(testFromString)
{
  calc::detail::FakeDevLicense fl;

  geo::FileCreateTester mt("tmp.res");
  PcrScript *s= pcr_createScriptFromTextString("tmp.res = inp1s.map + 4;");
  BOOST_CHECK(s);
  pcr_ScriptExecute(s);
  BOOST_CHECK(!pcr_ScriptError(s));
  BOOST_CHECK(mt.equalTo("inp5s.map",false));
  pcr_destroyScript(s);
}

// verbatim piece to copy into user docs
// just here to be sure it is compiled
#include "pcrcalc.h"
static void foo() {

    PcrScript *s=pcr_createScriptFromXMLFile("c:\\tmp\\case.xml");
    if (!s) {
      printf("PANIC allocation of a few bytes failed");
      abort();
    }
    if (pcr_ScriptError(s)) {
      /* typical error: case.xml is not existant */
      printf("ERROR: %s\n",pcr_ScriptErrorMessage(s));
      exit(1);
    }

    pcr_ScriptExecute(s);
    if (pcr_ScriptError(s)) {
      /* typical errors:
       * - case.xml is malformed
       * - some inputs are not found
       * - resource error: memory/disk full
       */
      printf("ERROR: %s\n",pcr_ScriptErrorMessage(s));
      exit(1);
    }

    pcr_destroyScript(s);
    if (pcr_ScriptError(s)) {
      /* very unlikely, program corruption
       */
      printf("ERROR: %s\n",pcr_ScriptErrorMessage(s));
      exit(1);
    }
}
// end of verbatim piece to copy into user docs

//! test the C-Api
BOOST_AUTO_TEST_CASE(testCapi)
{
  calc::detail::FakeDevLicense fl;

  { // NULL pointer to pcr_createScriptFromTextFile
    PcrScript *s=pcr_createScriptFromTextFile(0);
    BOOST_CHECK(s);
    BOOST_CHECK(pcr_ScriptError(s));
    std::string msg(pcr_ScriptErrorMessage(s));
    BOOST_CHECK(msg.find("pcr_createScriptFrom") != std::string::npos);
    pcr_destroyScript(s);
  }


  { // file not found
    const char *f="failureExpectedNotExistant";
    PcrScript *s=pcr_createScriptFromTextFile(f);
    BOOST_CHECK(s);


    // should trigger error: since file is not there
    // this messes up VS2005:
    const void *result = pcr_ScriptXMLReflection(s);
    BOOST_CHECK_EQUAL(result,(const void *)0);

    BOOST_CHECK(pcr_ScriptError(s));
    std::string msg(pcr_ScriptErrorMessage(s));
    BOOST_CHECK(msg.find(f) != std::string::npos);
    pcr_destroyScript(s);
  }

  { // some input not found
    com::write("piet.map = not_existant.map * 2;","pcrscripttest.mod");
    PcrScript *s=pcr_createScriptFromTextFile("pcrscripttest.mod");
    BOOST_CHECK(s);
    pcr_ScriptExecute(s);
    BOOST_CHECK(pcr_ScriptError(s));
    std::string msg(pcr_ScriptErrorMessage(s));
    BOOST_CHECK(msg.find("not_existant.map") != std::string::npos);
    pcr_destroyScript(s);
  }

  { // execute
    com::write("tmp.res = inp1s.map + 4;","pcrscripttest.mod");
    geo::FileCreateTester fct("tmp.res");
    PcrScript *s=pcr_createScriptFromTextFile("pcrscripttest.mod");
    BOOST_CHECK(s);
    pcr_ScriptExecute(s);
    BOOST_CHECK(!pcr_ScriptError(s));
    pcr_destroyScript(s);
    BOOST_CHECK(fct.equalTo("inp5s.map",false));
  }

  { // execute again
    com::write("tmp2.res = inp1s.map + 4;","pcrscripttest.mod");
    geo::FileCreateTester fct("tmp2.res");
    PcrScript *s=pcr_createScriptFromTextFile("pcrscripttest.mod");
    BOOST_CHECK(s);
    if (!s) foo(); // supress not used message of foo: the sample code
    pcr_ScriptExecute(s);
    BOOST_CHECK(!pcr_ScriptError(s));
    pcr_destroyScript(s);
    BOOST_CHECK(fct.equalTo("inp5s.map",false));
  }
  {
    PcrScript *s=pcr_createScriptFromTextString("result=nominal(b)");
    BOOST_CHECK(s != 0);
    const char *refl = pcr_ScriptXMLReflection(s);
    BOOST_CHECK(refl != 0);
    pcr_destroyScript(s);
  }
}

BOOST_AUTO_TEST_CASE(testIOMemoryStatic)
{
  calc::detail::FakeDevLicense fl;
  // with interface
  //    ASTTestFactory::modelFromId("pcrcalc521").c_str());

  // 0-ptr data, not ready to run
  void  *data0[2] = {0,0};

  { // 1) as is, default not memory IO, so pcr tries
    // find  input data as files
   PcrScript *s=pcr_createScriptFromXMLFile("apiExamples/memoryOnlyIO_1.xml");
   int r(pcr_ScriptExecuteInitialStepMemory(s, data0));
   BOOST_CHECK_EQUAL(r,-1);
   BOOST_CHECK(pcr_ScriptError(s));

   BOOST_CHECK(pcr_ScriptError(s));
   BOOST_CHECK_MESSAGE_ErrorMessage(s,
          "memInput: File 'memInput': No such file or directory");
   pcr_destroyScript(s);
  }
  { // 2)Memory IO, but no AreaMap set
    PcrScript *s=pcr_createScriptFromXMLFile("apiExamples/memoryOnlyIO_2.xml");
    pcr_ScriptExecuteInitialStepMemory(s, data0);
    BOOST_CHECK(pcr_ScriptError(s));
    BOOST_CHECK_MESSAGE_ErrorMessage(s, "no clone or area map specified");
    pcr_destroyScript(s);
  }
  { // 3) AreaMap set but result is ambiguos
    PcrScript *s=pcr_createScriptFromXMLFile("apiExamples/memoryOnlyIO_3.xml");
    pcr_ScriptExecuteInitialStepMemory(s, data0);
    BOOST_CHECK(pcr_ScriptError(s));
    BOOST_CHECK_MESSAGE_ErrorMessage(s,
          "memOutput: Use a conversion function to pick an output data type");
    pcr_destroyScript(s);
  }
  { // 4) Set a type for memInput, but the wrong one: only ordinal and scalar allowed
    PcrScript *s=pcr_createScriptFromXMLFile("apiExamples/memoryOnlyIO_4.xml");
    pcr_ScriptExecuteInitialStepMemory(s, data0);
    BOOST_CHECK(pcr_ScriptError(s));
    BOOST_CHECK_MESSAGE_ErrorMessage(s,
      "memInput: used as one of (scalar,ordinal) type on line");
    BOOST_CHECK_MESSAGE_ErrorMessage(s, "and set here as nominal type");
    pcr_destroyScript(s);
  }
  { // 5) Set a type for memInput, good one: but windowmaximum requires spatial
    PcrScript *s=pcr_createScriptFromXMLFile("apiExamples/memoryOnlyIO_5.xml");
    pcr_ScriptExecuteInitialStepMemory(s, data0);
    BOOST_CHECK(pcr_ScriptError(s));
    BOOST_CHECK_MESSAGE_ErrorMessage(s,
       "memInput: script requires spatial set here as nonspatial");
    pcr_destroyScript(s);
  }
  { // 6) Set a dataType for memInput: but IOMemory needs explicit spatial type
    //    since cover accepts both
    PcrScript *s=pcr_createScriptFromXMLFile("apiExamples/memoryOnlyIO_6.xml");
    int r(pcr_ScriptExecuteInitialStepMemory(s, data0));
    BOOST_CHECK_EQUAL(r,-1);
    BOOST_CHECK_MESSAGE_ErrorMessage(s,
    "ERROR: memInput: spatialType undecided (specify by field.spatialType");
    pcr_destroyScript(s);
  }
  { // 7) Runtime error passing 0-ptrs
    PcrScript *s=pcr_createScriptFromXMLFile("apiExamples/memoryOnlyIO_7.xml");
    BOOST_CHECK(!pcr_ScriptError(s));
    int r(pcr_ScriptExecuteInitialStepMemory(s, data0));
    BOOST_CHECK_EQUAL(r,-1);
    BOOST_CHECK(pcr_ScriptError(s));
    BOOST_CHECK_MESSAGE_ErrorMessage(s,
              "?:1:13:ERROR: memInput: 0-ptr data input buffer passed");
    pcr_destroyScript(s);
  }
  float input[25];
  std::fill(input,input+25,4.5F);
  { // X) Let API allocate memory
    PcrScript *s=pcr_createScriptFromXMLFile("apiExamples/memoryOnlyIO_7.xml");
    BOOST_CHECK(!pcr_ScriptError(s));
    void *data[2] = { input, 0 }; // output 0, means allocate by API
    int r(pcr_ScriptExecuteInitialStepMemory(s, data));
    BOOST_CHECK_EQUAL(r,0);
    BOOST_CHECK(!pcr_ScriptError(s));

    BOOST_CHECK(data[1]);
    const float *output=(const float *)data[1];
    BOOST_CHECK_EQUAL(output[1],7.5F);
    BOOST_CHECK_EQUAL(output[24],7.5F);
    pcr_destroyScript(s);
  }

  { // 8) Thunderbirds are go!
    float output[25];
    void  *data[2] = {input,output};
    std::fill(output,output+25,0.0F);
    PcrScript *s=pcr_createScriptFromXMLFile("apiExamples/memoryOnlyIO_7.xml");
    BOOST_CHECK(!pcr_ScriptError(s));

    int r(pcr_ScriptExecuteInitialStepMemory(s, data));
    BOOST_CHECK_EQUAL(r,0);
    BOOST_CHECK(!pcr_ScriptError(s));

    calc::ASTScript const& is(pcr_internalScript(s));
    BOOST_CHECK(!is.containsDynamicSection());
    BOOST_CHECK(is.symbols()["memOutput"].report() != 0);

    BOOST_CHECK_EQUAL(output[1],7.5F);
    BOOST_CHECK_EQUAL(output[24],7.5F);
    pcr_destroyScript(s);
  }
  { // return statistics as table
   // TODO verify
   //       cellsize in area result
    float output[25];
    void  *data[4] = {input,output,0 ,0}; // both 0's should be allocated
    std::fill(output,output+25,0.0F);
    PcrScript *s=pcr_createScriptFromXMLFile("apiExamples/memoryOnlyIO_StatisticsAsString.xml");
    BOOST_CHECK(!pcr_ScriptError(s));

    int r(pcr_ScriptExecuteInitialStepMemory(s, data));
    BOOST_CHECK_MESSAGE(!pcr_ScriptError(s), pcr_ScriptErrorMessage(s));
    BOOST_REQUIRE_EQUAL(r,0);

    BOOST_CHECK_EQUAL(output[1],7.5F);
    BOOST_CHECK_EQUAL(output[24],7.5F);

    calc::ASTScript const& is(pcr_internalScript(s));
    BOOST_CHECK(is.symbols().contains("memInput"));
    BOOST_CHECK(is.symbols().contains("memOutMap"));
    BOOST_CHECK(is.symbols().contains("memOutStr"));
    BOOST_CHECK(is.symbols().contains("memOutStr2"));

    BOOST_CHECK(data[2]); // allocated
    if (data[2])
    {
      std::string statTable((const char *)data[2]);
      BOOST_CHECK_EQUAL(statTable, std::string(
        "	memInput\n"
        "area	2500\n"
        "sum	112.5\n"
        "minimum	4.5\n"
        "maximum	4.5\n"
        "average	4.5\n"
        "standard deviation	0\n"
        "median	4.5\n"));
    }
    BOOST_CHECK(data[3]); // allocated
    if (data[3])
    {
      std::string statTable((const char *)data[3]);
      BOOST_CHECK_EQUAL(statTable, std::string(
        "	memOutMap\n"
        "area	2500\n"
        "sum	187.5\n"
        "minimum	7.5\n"
        "maximum	7.5\n"
        "average	7.5\n"
        "standard deviation	0\n"
        "median	7.5\n"));
    }
    pcr_destroyScript(s);
  }
  { // type clash
    //   type check clash if memOutput is used also for a field output
    void  *data[2] = {input,0};
    PcrScript *s=pcr_createScriptFromXMLFile("apiExamples/memoryOnlyIO_Error1.xml");
    BOOST_CHECK(!pcr_ScriptError(s));

    int r(pcr_ScriptExecuteInitialStepMemory(s, data));
    BOOST_CHECK(r!=1);
    BOOST_CHECK(pcr_ScriptError(s));
    BOOST_CHECK_MESSAGE_ErrorMessage(s,
    "xml:ERROR: memOutput: defined as statistics type on line '1:3' and set here as scalar type");
    bool todo(false);
    // see void calc::ASTSymbolInfo::setDefinition(const pcrxml::Definition& d)
    // has the way to do it 'the other way around'
    BOOST_WARN_MESSAGE(todo,
    "SHOULD BE"
    "file.xml:ERROR: memOutput: defined as scalar type on line '1:3' and set here as statistics type");
  }
  { // TODO todoAllowModelVarsToBeUsedInStatistics
    void  *data[2] = {input,0};
    PcrScript *s=pcr_createScriptFromXMLFile("apiExamples/memoryOnlyIO_Error2.xml");
    BOOST_CHECK(!pcr_ScriptError(s));

    int r(pcr_ScriptExecuteInitialStepMemory(s, data));
    BOOST_CHECK(r!=1);
    BOOST_CHECK(pcr_ScriptError(s));
    // ID attribute 'tmp' is referenced but was never declared
    BOOST_CHECK_MESSAGE_ErrorMessage(s,"ID attribute 'tmp'");

    bool todoAllowModelVarsToBeUsedInStatistics(false);
    BOOST_WARN(todoAllowModelVarsToBeUsedInStatistics);
  }
  { // writing to same output twice
    void  *data[2] = {input,0};
    PcrScript *s=pcr_createScriptFromXMLFile("apiExamples/memoryOnlyIO_Error3.xml");
    BOOST_CHECK(!pcr_ScriptError(s));

    int r(pcr_ScriptExecuteInitialStepMemory(s, data));
    BOOST_CHECK(r!=1);
    BOOST_CHECK(pcr_ScriptError(s));
    BOOST_CHECK_MESSAGE_ErrorMessage(s,
     "ERROR: redefinition of textStatistics with name 'memOutput'");
  }
/*
 *{ // 9) input and return a nonspatial
 *  PcrScript *s=pcr_createScriptFromTextString(sumModel);
 *  pcr_ScriptConfigure(s,"<exchangeModel id='dummy' ioStrategy='IOMemory'>"
 *                        " <areaMapDTD><rasterSpace nrRows='5' nrCols='5'/></areaMapDTD>"
 *                        " <exchangeItem><variable id='memInput' spatial='Yes'>"
 *                        "  <dataTypeDTD value='Scalar'/>"
 *                        " </variable></exchangeItem>"
 *                        " <exchangeItem><variable id='aNonSpatial' spatial='Non'>"
 *                        "  <dataTypeDTD value='Scalar'/>"
 *                        " </variable></exchangeItem>"
 *                        "</exchangeModel>"
 *                      );
 *  BOOST_CHECK(!pcr_ScriptError(s));

 *  // assert correct index and full specification in XMLReflection
 *  XMLReflection xr(pcr_ScriptXMLReflection(s));
 *  BOOST_CHECK(xr.exchangeModel().exchangeItem.size()==3);
 *  BOOST_CHECK(xr.exchangeModel().exchangeItem[0]->index() ==0);
 *  BOOST_CHECK(xr.exchangeModel().exchangeItem[0]->exchangeDirection.present());
 *  BOOST_CHECK(xr.exchangeModel().exchangeItem[0]->exchangeDirection()==
 *            pcrxml::ExchangeDirection::Input);
 *  BOOST_CHECK(xr.exchangeModel().exchangeItem[0]->variable->id()==
 *            "aNonSpatial");
 *  BOOST_CHECK(xr.exchangeModel().exchangeItem[1]->index() ==1);
 *  BOOST_CHECK(xr.exchangeModel().exchangeItem[1]->exchangeDirection.present());
 *  BOOST_CHECK(xr.exchangeModel().exchangeItem[1]->exchangeDirection()==
 *            pcrxml::ExchangeDirection::Input);
 *  BOOST_CHECK(xr.exchangeModel().exchangeItem[1]->variable->id()==
 *            "memInput");

 *  BOOST_CHECK(xr.exchangeModel().exchangeItem[2]->index() ==2);
 *  BOOST_CHECK(xr.exchangeModel().exchangeItem[2]->exchangeDirection.present());
 *  BOOST_CHECK(xr.exchangeModel().exchangeItem[2]->exchangeDirection()==
 *            pcrxml::ExchangeDirection::Output);
 *  BOOST_CHECK(xr.exchangeModel().exchangeItem[2]->variable->id()==
 *            "memOutput");

 *  float output=0;
 *  float aNonSpatialVal=2.0F;
 *  void  *data[3] = {&aNonSpatialVal,input,&output};

 *  pcr_ScriptExecuteInitialStepMemory(s, data);
 *  BOOST_CHECK(!pcr_ScriptError(s));
 *  BOOST_CHECK(6.5==output);
 *  pcr_destroyScript(s);
 *}
*/
//   { // new bug
//   [JIRA:Issues] (FEWS-5756) pcraster transformation crash on linux when reading external map file
//   niet ons probleem, het werkt.
//     REAL4 SRSbuf[40000];
//     void  *data[2] = {SRSbuf,0};
//     PcrScript *s=pcr_createScriptFromXMLFile("apiExamples/bug1.xml");
//     BOOST_CHECK(!pcr_ScriptError(s));
//
//     int r(pcr_ScriptExecuteInitialStepMemory(s, data));
//     BOOST_CHECK(!pcr_ScriptError(s));
//     for(int i=0; i !=365; ++i)
//     {
//       pcr_ScriptExecuteNextTimeStepMemory(s,data);
//       BOOST_CHECK(!pcr_ScriptError(s));
//     }
//     pcr_ScriptExecuteFinish(s);
//     BOOST_CHECK(!pcr_ScriptError(s));
//     pcr_destroyScript(s);
//   }
}

BOOST_AUTO_TEST_CASE(testIOMemoryDynamic)
{
  using namespace calc;

 enum {
  StaticInput=0,
  DynamicInput=1,
  MemInputRelation=2,
  MemOutputDynamic=3,
  MemOutputInitial=4,
  NrData=5
 };

  detail::FakeDevLicense fl;
  // with interface
  //    ASTTestFactory::modelFromId("pcrcalc521").c_str());
  {
   float  staticInput[1] = { 1 };

   float dynamicInput[25];
   std::fill(dynamicInput,dynamicInput+25,4.5F);

   float  memOutputInitial[1] = { -1 };

   float  memOutputDynamic[25];
   std::fill(memOutputDynamic,memOutputDynamic+25,-1.0F);

   void  *data[NrData] = {staticInput,
                          0, // not used in initial
                          0, // not used in initial
                          memOutputDynamic,
                          memOutputInitial
                          };

    // testdata/apiExamples/memoryOnlyIO_8.xml
    PcrScript *s=pcr_createScriptFromXMLFile("apiExamples/memoryOnlyIO_8.xml");
    BOOST_CHECK(!pcr_ScriptError(s));

    int r(pcr_ScriptExecuteInitialStepMemory(s, data));
    BOOST_CHECK_EQUAL(r,1);
    BOOST_REQUIRE(!pcr_ScriptError(s));

    BOOST_CHECK_EQUAL(memOutputInitial[0],5.0F);

    // memOutputDynamic is not assigned in initial
    // see \bug tag in API documentation of pcr_ScriptExecuteInitialStepMemory
    // bugzilla #104
    bool todoMemAssignInStatic=false;
    BOOST_WARN(todoMemAssignInStatic);
    BOOST_CHECK_EQUAL(memOutputDynamic[1],-1.0F);   // should NOT be -1 !!!
    BOOST_CHECK_EQUAL(memOutputDynamic[24],-1.0F);  // should NOT be -1 !!!

    UINT4  valueLookup[3] = { 1, 1, 10 };
    INT4  *value = (INT4 *)(valueLookup+2);
    *value = 10;

    // set what is used in dynamic
    data[DynamicInput] = dynamicInput;
    data[MemInputRelation] = valueLookup;

    // not earlier then after execute, since
    //  evaluation of scripts is only done at first execute
    ASTScript const& is(pcr_internalScript(s));
    BOOST_CHECK(is.containsDynamicSection());
    BOOST_CHECK(is.symbols()["memOutputDynamic"].report() != 0);

    pcr_ScriptExecuteNextTimeStepMemory(s, data);
    BOOST_CHECK_EQUAL(memOutputDynamic[1],13.5F);
    BOOST_CHECK_EQUAL(memOutputDynamic[24],13.5F);

    *value = 100;
    std::fill(dynamicInput,dynamicInput+25,8.0F);

    pcr_ScriptExecuteNextTimeStepMemory(s, data);
    BOOST_CHECK_EQUAL(memOutputDynamic[1],106.0F);

    pcr_destroyScript(s);
  }
  { // let API allocate memory
    // initial
    //  memOutputDynamic = staticInput * 1;
    //                     1
    //  memOutputInitial  = staticInput * 5;
    // dynamic
    //  memOutputDynamic = dynamicInput-time() + lookupscalar(memInputRelation,1);
   float  staticInput[1] = { 1 };

   float dynamicInput[25];
   std::fill(dynamicInput,dynamicInput+25,4.5F);

   float  memOutputDynamic[25];
   std::fill(memOutputDynamic,memOutputDynamic+25,-1.0F);

    UINT4  valueLookup[3] = { 1, 1, 10 };
    INT4  *value = (INT4 *)(valueLookup+2);
    *value = 10;

    void  *data[NrData] = {staticInput,
                           dynamicInput, // not used in initial
                           valueLookup, // not used in initial
                          0, // output to be allocated
                          0, // output to be allocated
                          };

    PcrScript *s=pcr_createScriptFromXMLFile("apiExamples/memoryOnlyIO_8.xml");
    BOOST_CHECK(!pcr_ScriptError(s));


    BOOST_CHECK(!data[MemOutputInitial]);
    int r(pcr_ScriptExecuteInitialStepMemory(s, data));
    BOOST_CHECK_EQUAL(r,1);
    BOOST_REQUIRE(!pcr_ScriptError(s));

    BOOST_CHECK(data[MemOutputInitial]);
    float* allocatedMemOutputInitial = (float *)data[MemOutputInitial];
    if (allocatedMemOutputInitial)
      BOOST_CHECK_EQUAL(allocatedMemOutputInitial[0],5.0F);

    BOOST_CHECK(!data[MemOutputDynamic]);
    pcr_ScriptExecuteNextTimeStepMemory(s, data);
    float *allocatedMemOutputDynamic = (float *)data[MemOutputDynamic];
    BOOST_CHECK(allocatedMemOutputDynamic);
    if (allocatedMemOutputDynamic) {
     BOOST_CHECK_EQUAL(allocatedMemOutputDynamic[1],13.5F);
     BOOST_CHECK_EQUAL(allocatedMemOutputDynamic[1],13.5F);
     BOOST_CHECK_EQUAL(allocatedMemOutputDynamic[24],13.5F);
    }

    *value = 100;
    std::fill(dynamicInput,dynamicInput+25,8.0F);

    data[MemOutputDynamic]=0; // reset to 0
    pcr_ScriptExecuteNextTimeStepMemory(s, data);
    allocatedMemOutputDynamic = (float *)data[MemOutputDynamic];
    BOOST_CHECK(allocatedMemOutputDynamic);

    if (allocatedMemOutputDynamic) {
     BOOST_CHECK_EQUAL(allocatedMemOutputDynamic[1],106.0F);
     BOOST_CHECK_EQUAL(allocatedMemOutputDynamic[1],106.0F);
     BOOST_CHECK_EQUAL(allocatedMemOutputDynamic[24],106.0F);
    }

    pcr_destroyScript(s);
  }
  { // array limitations runtime error
    UINT4  indexedArray[3] = { 2, 5, 5 };
    float  stub[25]; // not used because of error
    std::fill(stub,stub+25,1.0F);
    void  *data[NrData] = {stub,stub,indexedArray,stub,stub};
    PcrScript *s=pcr_createScriptFromXMLFile("apiExamples/memoryOnlyIO_8.xml");

    int r(pcr_ScriptExecuteInitialStepMemory(s, data));
    BOOST_CHECK_EQUAL(r,-1);
    BOOST_CHECK(pcr_ScriptError(s));

    BOOST_CHECK_MESSAGE_ErrorMessage(s,
          "?:9:59:ERROR: memInputRelation: Only 1 dimension supported");
    pcr_destroyScript(s);
  }
  { // passing 0 as array yields runtime error
    float  stub[25]; // not used because of error
    std::fill(stub,stub+25,1.0F);
    void  *data[NrData] = {stub,stub,0,stub,stub};

    PcrScript *s=pcr_createScriptFromXMLFile("apiExamples/memoryOnlyIO_8.xml");

    // no error since array is not used in initial section
    int r(pcr_ScriptExecuteInitialStepMemory(s, data));
    BOOST_CHECK_EQUAL(r,1);
    BOOST_CHECK(!pcr_ScriptError(s));

    // error in timestep 1 since array is used then
    pcr_ScriptExecuteNextTimeStepMemory(s, data);
    BOOST_CHECK(pcr_ScriptError(s));
    BOOST_CHECK_MESSAGE_ErrorMessage(s,
         "?:9:46:ERROR: RUNTIME (at timestep 1) 0-ptr data buffer passed");

    pcr_destroyScript(s);
  }
  { // calling pcr_ScriptExecuteInitialStepMemory twice
    float  stub[25]; // not used because of error
    std::fill(stub,stub+25,1.0F);
    void  *data[NrData] = {stub,stub,0,stub,stub};

    PcrScript *s=pcr_createScriptFromXMLFile("apiExamples/memoryOnlyIO_8.xml");

    int r(pcr_ScriptExecuteInitialStepMemory(s, data));
    BOOST_CHECK_EQUAL(r,1);
    BOOST_CHECK(!pcr_ScriptError(s));
    r=pcr_ScriptExecuteInitialStepMemory(s, data);
    BOOST_CHECK_EQUAL(r,-1);
    BOOST_CHECK(pcr_ScriptError(s));
    BOOST_CHECK_MESSAGE_ErrorMessage(s,
                 "pcr_ScriptExecuteInitialStepMemory called twice");
  }
  { //  pcr_ScriptExecuteNextTimeStepMemory() called with no prior call to pcr_ScriptExecuteInitialStepMemory
    float  stub[25]; // not used because of error
    std::fill(stub,stub+25,1.0F);
    void  *data[NrData] = {stub,stub,0,stub,stub};

    PcrScript *s=pcr_createScriptFromXMLFile("apiExamples/memoryOnlyIO_8.xml");

    int r=pcr_ScriptExecuteNextTimeStepMemory(s, data);
    BOOST_CHECK_EQUAL(r,-1);
    BOOST_CHECK(pcr_ScriptError(s));
    BOOST_CHECK_MESSAGE_ErrorMessage(s,
       "pcr_ScriptExecuteNextTimeStepMemory called with no prior call to pcr_ScriptExecuteInitialStepMemory");
  }
  { // dynamic section, no timer
    void  *data[NrData] = {0,0,0,0,0};
    // testdata/apiExamples/dynamicNoTimer.xml
    PcrScript *s=pcr_createScriptFromXMLFile("apiExamples/dynamicNoTimer.xml");

    pcr_ScriptExecuteInitialStepMemory(s, data);
    BOOST_CHECK(pcr_ScriptError(s));
    BOOST_CHECK_MESSAGE_ErrorMessage(s,
           "There is a dynamic section but no timer section");
    pcr_destroyScript(s);
  }
}

BOOST_AUTO_TEST_CASE(testIOMemoryTimeoutput)
{
  using namespace calc;

  detail::FakeDevLicense fl;
  {
    void  *data[1] = {0};

    // testdata/apiExamples/memoryOnlyIO_Timeoutput.xml
    PcrScript *s=pcr_createScriptFromXMLFile("apiExamples/memoryOnlyIO_Timeoutput.xml");
    BOOST_CHECK(!pcr_ScriptError(s));

    int r(pcr_ScriptExecuteInitialStepMemory(s, data));
    BOOST_CHECK_EQUAL(r,1);
    BOOST_CHECK(!pcr_ScriptError(s));
    BOOST_CHECK(!data[0]);

    r = pcr_ScriptExecuteNextTimeStepMemory(s, data);
    BOOST_CHECK_EQUAL(r,1);
    BOOST_REQUIRE(!pcr_ScriptError(s));
    BOOST_CHECK(data[0]);
    if (data[0])
    {
      const UINT4 *header = (const UINT4 *)data[0];
      BOOST_CHECK_EQUAL(header[0],(UINT4)1); // id
      BOOST_CHECK_EQUAL(header[1],(UINT4)CR_REAL4); // value type
      BOOST_CHECK_EQUAL(header[2],(UINT4)1); // nrDim
      BOOST_CHECK_EQUAL(header[3],(UINT4)5); // lenDim1

      const float *tss=(const float *)(header+4);
      BOOST_CHECK_EQUAL(tss[0],1);
      BOOST_CHECK_EQUAL(tss[1],2);
      BOOST_CHECK_EQUAL(tss[2],3);
      BOOST_CHECK_EQUAL(tss[3],4);
      BOOST_CHECK_EQUAL(tss[4],5);

    }
    data[0]=0;
    r = pcr_ScriptExecuteNextTimeStepMemory(s, data);
    BOOST_CHECK_EQUAL(r,0);
    BOOST_REQUIRE(!pcr_ScriptError(s));
    BOOST_CHECK(data[0]);
    if (data[0])
    {
      const UINT4 *header = (const UINT4 *)data[0];
      BOOST_CHECK_EQUAL(header[0],(UINT4)1); // id
      BOOST_CHECK_EQUAL(header[1],(UINT4)CR_REAL4); // value type
      BOOST_CHECK_EQUAL(header[2],(UINT4)1); // nrDim
      BOOST_CHECK_EQUAL(header[3],(UINT4)5); // lenDim1

      const float *tss=(const float *)(header+4);
      BOOST_CHECK_EQUAL(tss[0],2);
      BOOST_CHECK_EQUAL(tss[1],4);
      BOOST_CHECK_EQUAL(tss[2],6);
      BOOST_CHECK_EQUAL(tss[3],8);
      BOOST_CHECK_EQUAL(tss[4],10);
    }

    // extra should not do anthing
    r = pcr_ScriptExecuteNextTimeStepMemory(s, data);
    BOOST_CHECK_EQUAL(r,0);

    pcr_destroyScript(s);
  }
}

//! test settings from within xml
BOOST_AUTO_TEST_CASE(testXMLSettings)
{
  using namespace calc;

  detail::FakeDevLicense fl;
    UINT1 input[4]= { 1,0,0,1 };
    INT4 output[4] = { MV_INT4,MV_INT4,MV_INT4,MV_INT4 };

    void  *data[2] = {input,output};

  { // set to false in xml (not the default)

    PcrScript *s=pcr_createScriptFromXMLFile("apiExamples/diagonalSetFalse.xml");
    BOOST_CHECK(!pcr_ScriptError(s));
    pcr_ScriptExecuteInitialStepMemory(s, data);
    BOOST_CHECK(!pcr_ScriptError(s));

    BOOST_CHECK_EQUAL(output[0],1);
    BOOST_CHECK_EQUAL(output[1],2);
    BOOST_CHECK_EQUAL(output[2],3);
    BOOST_CHECK_EQUAL(output[3],4);
    pcr_destroyScript(s);
  }
  { // not set in the XML (default)

    PcrScript *s=pcr_createScriptFromXMLFile("apiExamples/diagonalNotSet.xml");
    BOOST_CHECK(!pcr_ScriptError(s));
    pcr_ScriptExecuteInitialStepMemory(s, data);
    BOOST_CHECK(!pcr_ScriptError(s));

    BOOST_CHECK_EQUAL(output[0],1);
    BOOST_CHECK_EQUAL(output[1],2);
    BOOST_CHECK_EQUAL(output[2],2);
    BOOST_CHECK_EQUAL(output[3],1);
    pcr_destroyScript(s);
  }
  { // set in XML

    PcrScript *s=pcr_createScriptFromXMLFile("apiExamples/diagonalSetTrue.xml");
    BOOST_CHECK(!pcr_ScriptError(s));

    pcr_ScriptExecuteInitialStepMemory(s, data);
    BOOST_CHECK(!pcr_ScriptError(s));

    BOOST_CHECK_EQUAL(output[0],1);
    BOOST_CHECK_EQUAL(output[1],2);
    BOOST_CHECK_EQUAL(output[2],2);
    BOOST_CHECK_EQUAL(output[3],1);
    pcr_destroyScript(s);
  }
}


BOOST_AUTO_TEST_CASE(testBil)
{
  using namespace calc;

  detail::FakeDevLicense fl;
  PcrScript *s(0);
  try { // execute again
    com::write("#! --bandmap\ntmp2.bil = inp1s.map + 4;","pcrscripttest.mod");
    // geo::FileCreateTester fct("tmp2.res");
    PcrScript *s=pcr_createScriptFromTextFile("pcrscripttest.mod");
    BOOST_CHECK(s);
    pcr_ScriptExecute(s);
    BOOST_CHECK(appIOstrategy==APP_IO_BANDMAP);
    BOOST_CHECK_MESSAGE(!pcr_ScriptError(s),std::string(pcr_ScriptErrorMessage(s)));
    pcr_destroyScript(s);
    // bin/linux-develop/testdir/tmp2.bil
    // bin/linux-develop/testdir/tmp2.hdr
    BOOST_CHECK(com::exists("tmp2.bil"));
    BOOST_CHECK(com::exists("tmp2.hdr"));
    // BOOST_CHECK(fct.equalTo("inp5s.map",false));
  } catch (...) {
    appIOstrategy=APP_IO_PCRASTER;
    pcr_destroyScript(s);
    throw;
 }
 appIOstrategy=APP_IO_PCRASTER;
 pcr_destroyScript(s);
}

BOOST_AUTO_TEST_CASE(testXML)
{
  using namespace calc;

  detail::FakeDevLicense fl;

  {
   PcrScript *s=pcr_createScriptFromXMLFile("PCRasterLinkOutTestPyDynamicModel.xml");
   pcr_ScriptExecute(s);

   BOOST_CHECK(pcr_ScriptError(s));
   BOOST_CHECK_MESSAGE_ErrorMessage(s,
    "pcr_ScriptExecute can not execute a script with memoryExchange elements");
  }

  {
   PcrScript *s=pcr_createScriptFromXMLFile("apiExamples/notValid.xml");
   pcr_ScriptExecute(s);

   BOOST_CHECK(pcr_ScriptError(s));
   // TODO DOMInput should reformat messages
   // no declaration found for element 'textModel'
   BOOST_CHECK_MESSAGE_ErrorMessage(s, "element 'textModel'");
  }
  {
   PcrScript *s=pcr_createScriptFromXMLFile("apiExamples/compressionNoAreaMap.xml");
   pcr_ScriptExecute(s);

   BOOST_CHECK(pcr_ScriptError(s));
   BOOST_CHECK_MESSAGE_ErrorMessage(s,"no clone or area map");
  }
  {
   PcrScript *s=pcr_createScriptFromXMLFile("apiExamples/compression.xml");

   Spatial::resetBPC();
   pcr_ScriptExecute(s);

#ifdef DEBUG_DEVELOP
   // with compression this is true:
   std::set<size_t> sizesChecked;
   sizesChecked.insert(33); // compressed
   sizesChecked.insert(36); // uncompressed
   BOOST_CHECK(Spatial::d_sizes == sizesChecked);
#endif

   BOOST_CHECK(!pcr_ScriptError(s));
   BOOST_CHECK(compareFileWithValidated("compressionResult.map"));
  }

  // test xsd not found
  bool skipValidatingIfSchemaIsNotFound=true;
  BOOST_WARN(skipValidatingIfSchemaIsNotFound);
}

BOOST_AUTO_TEST_CASE(testXMLHabitat)
{
  using namespace calc;

  detail::FakeDevLicense fl;
  { // ECOTOOP (was habitat8.xml)
    PcrScript *s=pcr_createScriptFromXMLFile("apiExamples/lookup.xml");
    pcr_ScriptExecute(s);
    BOOST_CHECK(pcr_ScriptErrorMessage(s)==std::string());
    pcr_destroyScript(s);

    BOOST_CHECK(detail::validateBil("res10_ecoclas"));
  }
  { // MISC (was habitat1.xml)
    PcrScript *s=pcr_createScriptFromXMLFile("apiExamples/staticScript.xml");
    pcr_ScriptExecute(s);
    BOOST_CHECK(pcr_ScriptErrorMessage(s)==std::string());
    pcr_destroyScript(s);
    BOOST_CHECK(detail::validateBil("res1_y"));
    BOOST_CHECK(detail::validateBil("res1_out"));
    // intermediates that should NOT be written
    BOOST_CHECK(!com::exists("q_tmp.bil"));
    BOOST_CHECK(!com::exists("r_tmp.bil"));
  }
}

BOOST_AUTO_TEST_CASE(testXMLStatistics)
{
  using namespace calc;

  detail::FakeDevLicense fl;
  { // statistics with mask
    PcrScript *s=pcr_createScriptFromXMLFile("apiExamples/statisticsMask.xml");
    StatTable::setVerbose(true);
    pcr_ScriptExecute(s);
    StatTable::setVerbose(false);
    BOOST_CHECK(pcr_ScriptErrorMessage(s)==std::string());
    BOOST_CHECK(compareFileWithValidated("statisticsMask.txt"));
    pcr_destroyScript(s);
  }
  { // Tue Mar 27 2007 solved BUG
    PcrScript *s=pcr_createScriptFromXMLFile("apiExamples/bilStatistics.xml");
    pcr_ScriptExecute(s);
    BOOST_CHECK(pcr_ScriptErrorMessage(s)==std::string());
    BOOST_CHECK(compareFileWithValidated("ecotoop2Bil.txt"));
    pcr_destroyScript(s);
  }
  { // Bugzilla #77
    // does not pick up the areaMap definition
    PcrScript *s=pcr_createScriptFromXMLFile("apiExamples/areaMap.xml");
    pcr_ScriptExecute(s);
    BOOST_CHECK(pcr_ScriptErrorMessage(s)==std::string());
    // BOOST_CHECK(compareFileWithValidated("ecotoop2Bil.txt"));
    pcr_destroyScript(s);
  }
  {
    StatTable::setVerbose(true);
    PcrScript *s=pcr_createScriptFromXMLFile("apiExamples/allStatistics.xml");
    pcr_ScriptExecute(s);
    StatTable::setVerbose(false);
    BOOST_CHECK(pcr_ScriptErrorMessage(s)==std::string());
    fs::directory_iterator end;
    for(fs::directory_iterator i("allStatisticsResults"); i != end; ++i ) {
      fs::path computed =fs::path("allStatisticsResults")/i->path().filename();
      BOOST_CHECK(compareFileWithValidated(computed.string()));
      // fs::path validated=fs::path("validated")/i->path().filename();
      //   if(!com::filesExistsAndEqual(validated.string(),computed.string()))
      //   {
      //     BOOST_CHECK(false);
      //   }
    }
    pcr_destroyScript(s);
  }
#ifdef __linux__
    bool writingBilReadingCsfMakeDalUnstable=true;
    BOOST_WARN(writingBilReadingCsfMakeDalUnstable);
#endif
}


BOOST_AUTO_TEST_CASE(testInit)
{
  {
    PcrScript *s= pcr_createScriptFromTextString("tmp.res = inp1s.map + 4;");
    BOOST_CHECK(s);
    pcr_ScriptExecute(s);
    BOOST_CHECK(!pcr_ScriptError(s));
    pcr_destroyScript(s);
  }
}
