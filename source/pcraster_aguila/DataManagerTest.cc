#define BOOST_TEST_MODULE pcraster aguila data_manager
#include <boost/test/unit_test.hpp>
#include <QCoreApplication>
#include "dev_GDalClient.h"
#include "dev_QtClient.h"
#include "dal_Client.h"
#include "ag_DataManager.h"
#include "ag_Raster.h"


static int argc = 1;
static char const* argv[1] = { "/my/path/data_manager_test" };

struct Fixture:
    private dev::GDalClient,
    private dev::QtClient<QCoreApplication>,
    private dal::Client
{

    Fixture()
        : dev::GDalClient(),
          dev::QtClient<QCoreApplication>(argc, const_cast<char**>(argv)),
          dal::Client(argv[0], true)
    {
    }

    ~Fixture()=default;

};


BOOST_GLOBAL_FIXTURE(Fixture);

BOOST_AUTO_TEST_CASE(test)
{
  using namespace ag;

  std::string name("dataset1/aap/scalar");
  dal::DataSpace space;
  space.addDimension(dal::Dimension(dal::Time, size_t(10), size_t(20),
         size_t(1)));

  Raster raster(name, space);
  DataInfo<Raster> data(&raster, VS_SCALAR);

  DataManager<Raster> manager(geo::STACK);
  BOOST_CHECK_EQUAL(manager.size(), size_t(0));
  BOOST_CHECK(manager.empty());

  DataGuide guide1 = manager.add(data);
  BOOST_CHECK_EQUAL(manager.size(), size_t(1));
  BOOST_CHECK(!manager.empty());

  // 2nd guide is pointing to the same data as the first.
  DataGuide guide2 = manager.add(data);
  BOOST_CHECK_EQUAL(manager.size(), size_t(1));
  BOOST_CHECK(!manager.empty());

  BOOST_CHECK(guide1 == guide2);

  BOOST_CHECK(manager.isValid(guide1));
  BOOST_CHECK(manager.exists(&raster));
  BOOST_CHECK_EQUAL(&manager.data(guide1), &raster);

  manager.remove(guide1);

  BOOST_CHECK_EQUAL(manager.size(), size_t(0));
  BOOST_CHECK(manager.empty());
}
