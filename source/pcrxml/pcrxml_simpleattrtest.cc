#define BOOST_TEST_MODULE pcraster pcrxml simple_attr
#include <boost/test/unit_test.hpp>
#include "pcrgenxml_directorystackinfo.h"
#include "pcrxml_document.h"

//! test all of Simple Attr template
BOOST_AUTO_TEST_CASE(it)
{
  using namespace pcrxml;

  Document const doc("<DirectoryStackInfo "
                     "allMissingValue='false' "
                     "minimumValue='1.45' "
                     "maximumValue='2.67' "
                     "dataTypeDTD='Scalar' "
                     "stackEnd='3' />");
  DirectoryStackInfo const d(doc.documentElement());
  BOOST_TEST(!d.allMissingValue());
  BOOST_TEST(d.minimumValue() == 1.45);
  BOOST_TEST(d.maximumValue() == 2.67);
  BOOST_TEST(d.dataTypeDTD() == DataTypeEnum::Scalar);
  BOOST_TEST(d.stackEnd() == 3);
}
