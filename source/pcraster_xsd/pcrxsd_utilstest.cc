#define BOOST_TEST_MODULE pcraster xsd utils
#include <boost/test/unit_test.hpp>
#include "pcrxsd_library.h"
#include "pcrxsd_utils.h"


using Fixture = pcrxsd::Library;

BOOST_GLOBAL_FIXTURE(Fixture);

BOOST_AUTO_TEST_CASE(contents_is_xml_or_pcraster_file_format)
{
  using namespace pcrxsd;

  BOOST_CHECK(contentsIsXMLOrPCRasterFileFormat("<?pi ?> \n <empty/>")=="empty");
  BOOST_CHECK(contentsIsXMLOrPCRasterFileFormat("\n<?pi ?> \n <empty/>")=="empty");
  BOOST_CHECK(contentsIsXMLOrPCRasterFileFormat("empty").empty());
  BOOST_CHECK(contentsIsXMLOrPCRasterFileFormat("< >").empty());
}
