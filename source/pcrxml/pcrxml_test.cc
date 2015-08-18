#include "stddefx.h"


#ifndef INCLUDED_BOOST_TEST_INCLUDED_UNIT_TEST
#include <boost/test/included/unit_test.hpp>
#define INCLUDED_BOOST_TEST_INCLUDED_UNIT_TEST
#endif

#ifndef INCLUDED_PCRXML_ENUMNMTOKENTEST
#include "pcrxml_enumnmtokentest.h"
#define INCLUDED_PCRXML_ENUMNMTOKENTEST
#endif

#ifndef INCLUDED_PCRXML_DOCUMENTTEST
#include "pcrxml_documenttest.h"
#define INCLUDED_PCRXML_DOCUMENTTEST
#endif

#ifndef INCLUDED_PCRXML_WORDTEST
#include "pcrxml_wordtest.h"
#define INCLUDED_PCRXML_WORDTEST
#endif

#ifndef INCLUDED_PCRXML_INTERSPACEDSENTENCETEST
#include "pcrxml_interspacedsentencetest.h"
#define INCLUDED_PCRXML_INTERSPACEDSENTENCETEST
#endif


#ifndef INCLUDED_PCRXML_CHILDELEMENTVISITORTEST
#include "pcrxml_childelementvisitortest.h"
#define INCLUDED_PCRXML_CHILDELEMENTVISITORTEST
#endif

#ifndef INCLUDED_PCRXML_ELEMENTTEST
#include "pcrxml_elementtest.h"
#define INCLUDED_PCRXML_ELEMENTTEST
#endif

#ifndef INCLUDED_PCRXML_ATTRIBUTETEST
#include "pcrxml_attributetest.h"
#define INCLUDED_PCRXML_ATTRIBUTETEST
#endif

#ifndef INCLUDED_PCRXML_BINDOUBLELETEST
#include "pcrxml_bindoubleletest.h"
#define INCLUDED_PCRXML_BINDOUBLELETEST
#endif
#ifndef INCLUDED_PCRXML_DOMALGORITHMTEST
#include "pcrxml_domalgorithmtest.h"
#define INCLUDED_PCRXML_DOMALGORITHMTEST
#endif
#ifndef INCLUDED_PCRXML_DOMTEST
#include "pcrxml_domtest.h"
#define INCLUDED_PCRXML_DOMTEST
#endif
#ifndef INCLUDED_PCRXML_PCDATAELEMENTTEST
#include "pcrxml_pcdataelementtest.h"
#define INCLUDED_PCRXML_PCDATAELEMENTTEST
#endif

#ifndef INCLUDED_PCRXML_SIMPLEATTRTEST
#include "pcrxml_simpleattrtest.h"
#define INCLUDED_PCRXML_SIMPLEATTRTEST
#endif



boost::unit_test::test_suite* init_unit_test_suite(int /* argc */, char ** const /*argv*/) {
  boost::unit_test::test_suite* test = BOOST_TEST_SUITE(__FILE__);

  test->add(pcrxml::DomTest().suite());
  test->add(pcrxml::AttributeTest().suite());
  test->add(pcrxml::SimpleAttrTest().suite());
  test->add(pcrxml::ChildElementVisitorTest().suite());
  test->add(pcrxml::DocumentTest().suite());
  test->add(pcrxml::ElementTest().suite());
  test->add(pcrxml::WordTest().suite());
  test->add(pcrxml::EnumNmTokenTest().suite());
  test->add(pcrxml::InterSpacedSentenceTest().suite());
  test->add(pcrxml::BinDoubleLETest().suite());
  test->add(pcrxml::PCDATAElementTest().suite());
  test->add(pcrxml::DomAlgorithmTest().suite());

  return test;
}
