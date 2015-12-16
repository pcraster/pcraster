#define BOOST_TEST_MODULE pcraster geo raster_file_converter
#include <boost/test/unit_test.hpp>
#include "com_pathname.h"
#include "geo_rasterfileconverter.h"


BOOST_AUTO_TEST_CASE(bil2ascii)
{
  using namespace geo;

  RasterFileConverter b2a("all1_float.bil");
  b2a.writeAscii("bil2ascii.txt");
}
