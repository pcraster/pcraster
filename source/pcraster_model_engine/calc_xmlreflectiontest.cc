#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_XMLREFLECTIONTEST
#include "calc_xmlreflectiontest.h"
#define INCLUDED_CALC_XMLREFLECTIONTEST
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

#ifndef INCLUDED_QDOM
#include <qdom.h>
#define INCLUDED_QDOM
#endif

// PCRaster library headers.
#ifndef INCLUDED_CALC_COMPAREFILEWITHVALIDATED
#include "calc_comparefilewithvalidated.h"
#define INCLUDED_CALC_COMPAREFILEWITHVALIDATED
#endif

// Module headers.
#ifndef INCLUDED_CALC_GLOBALLIBDEFS
#include "calc_globallibdefs.h"
#define INCLUDED_CALC_GLOBALLIBDEFS
#endif
#ifndef INCLUDED_CALC_ASTTESTFACTORY
#include "calc_asttestfactory.h"
#define INCLUDED_CALC_ASTTESTFACTORY
#endif
#ifndef INCLUDED_CALC_ASTSCRIPT
#include "calc_astscript.h"
#define INCLUDED_CALC_ASTSCRIPT
#endif
#ifndef INCLUDED_CALC_XMLREFLECTION
#include "calc_xmlreflection.h"
#define INCLUDED_CALC_XMLREFLECTION
#endif

/*!
  \file
  This file contains the implementation of the XMLReflectionTest class.
*/

// NOTE use string failureExpected in files expected to fail, see style guide


#ifndef INCLUDED_COM_FILE
#include "com_file.h"
#define INCLUDED_COM_FILE
#endif

namespace calc {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC XMLREFLECTION MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*XMLReflectionTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<XMLReflectionTest> instance(new XMLReflectionTest());

  suite->add(BOOST_CLASS_TEST_CASE(&XMLReflectionTest::testXMLReflection, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&XMLReflectionTest::testEsriXMLReflection, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF XMLREFLECTION MEMBERS
//------------------------------------------------------------------------------

//! ctor
XMLReflectionTest::XMLReflectionTest(
         )
{
}



//! setUp
void XMLReflectionTest::setUp()
{
 globalInit();
}



//! tearDown
void XMLReflectionTest::tearDown()
{
 globalEnd();
}

void XMLReflectionTest::testXMLReflection()
{
  {
    std::auto_ptr<ASTScript>s(ASTTestFactory::createFromIdOrStr("pcrcalc510a"));
    s->analyzeNoContext();
    XMLReflection xr(*s);
    com::write(xr.toString(),"pcrcalc510a.xml");
    BOOST_CHECK(compareFileWithValidated("pcrcalc510a.xml"));
/*
*   std::string xmlStr=s->xmlReflection();

*   pcrxml::Document dom(xmlStr.c_str());
*   pcrxml::ExchangeModel em(dom.documentElement());
*   XMLReflection xml(em);
*   BOOST_CHECK(xml.inputs().size()==1);
*   BOOST_CHECK(xml.outputs().size()==1);
*   BOOST_CHECK(0 == em.areaMapDTD);
*/
  }
  {
    const char *str=" initial dist = spread(cover(cone,0),0,1)+1; dem =  1/dist; report magmaPipe = dist < 300; sin45 = sin(45); dynamic network = lddcreate(dem, 1E35, 1E35, 1E35, 1E35); lavaFlow = if(magmaPipe, max(normal(1) * 5 + lavaLoad, 0), 0); sinusSlope = min(sin(atan(slope(dem))), sin45); transportCapacity = 0.9 + sinusSlope * 2 * sin45; flux = accuflux(network, lavaFlow * transportCapacity); flux *= depthConversion * if(magmaPipe, 0.15, 1); report dem += flux + windowaverage(normal(1) * 0.2, 120); report lava = if(flux > 0, flux);";
    std::auto_ptr<ASTScript>s(ASTTestFactory::createFromIdOrStr(str));
    s->analyzeNoContext();
    XMLReflection xr(*s);
    com::write(xr.toString(),"volcano.xml");
  }
/*
 *{
 *  ASTScript *s=ASTTestFactory::createFromIdOrStr("pcrcalc509");
 *  s->analyzeNoContext();
 *  std::string xmlStr=s->xmlReflection();
 *  BOOST_CHECK(!xmlStr.empty());

 *  pcrxml::Document dom(xmlStr.c_str());
 *  pcrxml::ExchangeModel em(dom.documentElement());

 *  XMLReflection xml(em);
 *  BOOST_CHECK(xml.inputs().size()==1);
 *  // output
 *  BOOST_CHECK(xml.outputs().empty()==true);

 *  BOOST_CHECK(0 == em.areaMapDTD);

 *  BOOST_CHECK(em.id()=="PCRaster/OpenMI TEST");
 *  BOOST_CHECK(em.exchangeItem.size()==1);
 *  BOOST_CHECK(em.exchangeItem[0]->index()==0);
 *  BOOST_CHECK(em.exchangeItem[0]->exchangeDirection()==
 *      pcrxml::ExchangeDirection::Input);

 *  BOOST_CHECK(0  ==xml.inputs()[0].index);
 *  BOOST_CHECK("Q"==xml.inputs()[0].id);

 *  BOOST_CHECK(em.exchangeItem[0]->variable->id()=="Q");
 *  BOOST_CHECK(em.exchangeItem[0]->variable->input() ==pcrxml::ModelInputType::Initial);
 *  BOOST_CHECK(em.exchangeItem[0]->variable->output()==pcrxml::ModelOutputType::Fixed);
 *  BOOST_CHECK(em.exchangeItem[0]->variable->spatial()==
 *      pcrxml::Spatial::Either);
 *  BOOST_CHECK(em.exchangeItem[0]->variable->description()=="water discharge");
 *  BOOST_CHECK(em.exchangeItem[0]->variable->dataTypeDTD[0]->value() ==
 *            pcrxml::DataTypeEnum::Scalar);
 *  BOOST_CHECK(em.exchangeItem[0]->variable->dataTypeDTD[0]->dimension.size()==2);
 *  BOOST_CHECK(em.exchangeItem[0]->variable->dataTypeDTD[0]->dimension[0]->base()== pcrxml::DimensionBaseEnum::Length);
 *  BOOST_CHECK(em.exchangeItem[0]->variable->dataTypeDTD[0]->dimension[0]->power()==3);

 *  BOOST_CHECK(em.exchangeItem[0]->variable->dataTypeDTD[0]->dimension[1]->base()== pcrxml::DimensionBaseEnum::Time);
 *  BOOST_CHECK(em.exchangeItem[0]->variable->dataTypeDTD[0]->dimension[1]->power()==-1);
 *  delete s;
 *}

 *{
 *  std::auto_ptr<ASTScript>s(ASTTestFactory::createFromIdOrStr("pcrcalc517"));
 *  s->analyzeNoContext();
 *  std::string xmlStr=s->xmlReflection();
 *  BOOST_CHECK(!xmlStr.empty());

 *  pcrxml::Document dom(xmlStr.c_str());
 *  pcrxml::ExchangeModel em(dom.documentElement());
 *  XMLReflection xml(em);

 *  BOOST_CHECK(1==xml.inputs().size());
 *  BOOST_CHECK(1==xml.outputs().size());

 *  bool areaMapReadingForIOConfigurable=false;
 *  BOOST_WARN(areaMapReadingForIOConfigurable);

 *  BOOST_CHECK(em.areaMapDTD);
 *  BOOST_CHECK(5 == em.areaMapDTD->rasterSpace->nrRows());
 *  BOOST_CHECK(5 == em.areaMapDTD->rasterSpace->nrCols());
 *  BOOST_CHECK(50== em.areaMapDTD->rasterSpace->cellSize());
 *  BOOST_CHECK(200 == em.areaMapDTD->rasterSpace->xLowerLeftCorner());
 *  BOOST_CHECK(-50 == em.areaMapDTD->rasterSpace->yLowerLeftCorner());
 *  BOOST_CHECK(0 == em.areaMapDTD->rasterMask);

 *  BOOST_CHECK(em.id()=="PCRaster/OpenMI TEST");
 *  BOOST_CHECK(em.exchangeItem.size()==2);
 *  BOOST_CHECK(em.exchangeItem[0]->index()==0);
 *  BOOST_CHECK(em.exchangeItem[0]->exchangeDirection()==
 *      pcrxml::ExchangeDirection::Input);
 *  BOOST_CHECK(0==xml.inputs()[0].index);
 *  BOOST_CHECK(em.exchangeItem[1]->index()==1);
 *  BOOST_CHECK(em.exchangeItem[1]->exchangeDirection()==
 *      pcrxml::ExchangeDirection::Output);
 *  BOOST_CHECK(1==xml.outputs()[0].index);

 *  pcrxml::Variable *v= em.exchangeItem[0]->variable;
 *  BOOST_CHECK(v);

 *  BOOST_CHECK(v->id()     =="inputMap");
 *  BOOST_CHECK(v->input()  == pcrxml::ModelInputType::Dynamic);
 *  BOOST_CHECK(v->output() == pcrxml::ModelOutputType::Fixed);
 *  BOOST_CHECK(v->spatial()== pcrxml::Spatial::Either);
 *  BOOST_CHECK(v->description()==
 *   "Provide any input map here for this algorithm");
 *  BOOST_CHECK(v->dataTypeDTD[0]->value() == pcrxml::DataTypeEnum::Scalar);
 *  BOOST_CHECK(v->dataTypeDTD[0]->dimension.size()==0);

 *  v= em.exchangeItem[1]->variable;
 *  BOOST_CHECK(v);
 *  BOOST_CHECK(v->id()     =="outputMap");
 *  BOOST_CHECK(v->input()  == pcrxml::ModelInputType::None);
 *  BOOST_CHECK(v->output() == pcrxml::ModelOutputType::Dynamic);
 *  BOOST_CHECK(v->spatial()== pcrxml::Spatial::Either);
 *  BOOST_CHECK(v->dataTypeDTD[0]->value() == pcrxml::DataTypeEnum::Scalar);
 *  BOOST_CHECK(v->dataTypeDTD[0]->dimension.size()==2);
 *  BOOST_CHECK(v->dataTypeDTD[0]->dimension[0]->base()== pcrxml::DimensionBaseEnum::Length);
 *  BOOST_CHECK(v->dataTypeDTD[0]->dimension[0]->power()==3);
 *  BOOST_CHECK(v->dataTypeDTD[0]->dimension[1]->base()== pcrxml::DimensionBaseEnum::Time);
 *  BOOST_CHECK(v->dataTypeDTD[0]->dimension[1]->power()==-1);
 *}
 */
}

void XMLReflectionTest::testEsriXMLReflection()
{
/*
  bool esriXMLReflectionAgain=true;
  BOOST_WARN(esriXMLReflectionAgain);
  {
    ASTScript *s=ASTTestFactory::createFromIdOrStr("pcrcalc533");
    s->analyzeNoContext();
    std::string xmlStr=s->xmlReflection();
    XMLReflection xr(xmlStr);

    // not in the script
    BOOST_CHECK(xr["notExistant"]==0);
    // in the script
    BOOST_CHECK(xr["lookupTable"]!=0);
    // in the script, but not exposed
    BOOST_CHECK(xr["a"]          ==0);

    // com::write(xmlStr,"esri.xml");
  }
  {
    ASTScript *s=ASTTestFactory::createFromIdOrStr(
                " report outputMap = memInput+3;");
    s->analyzeNoContext();
    std::string xmlStr=s->xmlReflection();
    XMLReflection xr(xmlStr);
    BOOST_CHECK(xr["outputMap"]!=0);
    BOOST_CHECK(xr["memInput"]!=0);
    BOOST_CHECK(xr.inputs().size()==1);
    BOOST_CHECK(xr.inputs()[0].id=="memInput");
    BOOST_CHECK(xr.outputs().size()==1);
    BOOST_CHECK(xr.outputs()[0].id=="outputMap");
  }
*/
}

} // namespace calc
