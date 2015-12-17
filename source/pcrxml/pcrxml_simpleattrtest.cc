#define BOOST_TEST_MODULE pcraster pcrxml simple_attr
#include <boost/test/unit_test.hpp>
#include "pcrgenxml_directorystackinfo.h"
#include "pcrxml_document.h"


//! test all of Simple Attr template
BOOST_AUTO_TEST_CASE(it)
{
  using namespace pcrxml;

      Document doc("<DirectoryStackInfo "
                     "allMissingValue='false' "
                     "minimumValue='1.45' "
                     "maximumValue='2.67' "
                     "dataTypeDTD='Scalar' "
                     "stackEnd='3' />");
      DirectoryStackInfo d(doc.documentElement());
      BOOST_CHECK(!d.allMissingValue());
      BOOST_CHECK(d.minimumValue()==1.45);
      BOOST_CHECK(d.maximumValue()==2.67);
      BOOST_CHECK(d.dataTypeDTD()==DataTypeEnum::Scalar);
      BOOST_CHECK(d.stackEnd()==3);
}
