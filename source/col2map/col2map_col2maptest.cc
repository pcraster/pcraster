#define BOOST_TEST_MODULE pcraster col2map
#include <boost/test/unit_test.hpp>
#include <cassert>
#include "dal_Client.h"
#include "dal_CSFRasterDriver.h"


static int const argc = 1;
static char const* argv[1] = { "/my/path/col2map_test" };


struct Fixture:
    private dal::Client
{

    Fixture()
        : dal::Client(argv[0], true)
    {
    }

    ~Fixture()=default;

};


BOOST_GLOBAL_FIXTURE(Fixture);


BOOST_AUTO_TEST_CASE(NaN)
{
  // http://pcrserver.geo.uu.nl/bugzilla/show_bug.cgi?id=247
  // nan.map is created in testrun.prolog.
  // Test whether a NaN on the input is converted to a missing value on the
  // output.
  // Status not yet correct: also test under DIFFERENT compilers if the file
  // reading already detets a string nan as a NAN, seems not standarized
//  #ifdef _MSC_VER
//    BOOST_WARN_MESSAGE( 0, "MSC does not read Nan (std? or gcc non-std)?");
//  #else
  dal::CSFRasterDriver driver;
  std::string filename("nan.map");
  dal::Raster* raster = dynamic_cast<dal::RasterDriver&>(driver).read(filename);

  assert(raster);
  assert(raster->typeId() == dal::TI_REAL4);

  BOOST_CHECK(!pcr::isMV(raster->cell<REAL4>(0)));
  BOOST_CHECK_EQUAL(raster->cell<REAL4>(0), 1.0F);
  //cuTodo(pcr::isMV(raster->cell<REAL4>(1)));
  BOOST_WARN(pcr::isMV(raster->cell<REAL4>(1)));
  BOOST_CHECK(!pcr::isMV(raster->cell<REAL4>(2)));
  BOOST_CHECK_EQUAL(raster->cell<REAL4>(2), 3.0F);
//  #endif
}
