#define BOOST_TEST_MODULE pcraster aguila data_object_base
#include <boost/test/unit_test.hpp>
#include <QCoreApplication>
#include "dev_GDalClient.h"
#include "dev_QtClient.h"
#include "dal_Client.h"
#define private public
#include "ag_RasterDataSources.h"


static int argc = 1;
static char const* argv[1] = { "/my/path/data_object_base_test" };

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

};


BOOST_GLOBAL_FIXTURE(Fixture);

BOOST_AUTO_TEST_CASE(test)
{
  using namespace ag;

  std::string name("dataset1/aap/scalar");
  dal::DataSpace space;
  space.addDimension(dal::Dimension(dal::Time, size_t(10), size_t(20),
         size_t(1)));

  RasterDataSources dataSources;
  BOOST_CHECK_EQUAL(dataSources.size(), size_t(0));
  BOOST_CHECK(dataSources.empty());

  DataGuide guide1 = dataSources.add(name, space);
  BOOST_CHECK_EQUAL(dataSources.size(), size_t(1));
  BOOST_CHECK(!dataSources.empty());

  DataGuide guide2 = dataSources.add(name, space);
  BOOST_CHECK_EQUAL(dataSources.size(), size_t(1));
  BOOST_CHECK(!dataSources.empty());

  BOOST_CHECK_EQUAL(dataSources._manager.size(), size_t(1));

  dataSources.clear();
  BOOST_CHECK_EQUAL(dataSources.size(), size_t(0));
  BOOST_CHECK(dataSources.empty());

  BOOST_CHECK(dataSources._manager.empty());
}
