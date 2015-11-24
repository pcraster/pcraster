#define BOOST_TEST_MODULE pcrblock block
#include <boost/test/unit_test.hpp>
#include "dal_MathUtils.h"
#include "discr_block.h"
#include "discr_blockdata.h"
#include "discr_raster.h"
#include "discr_rasterdata.h"
#include "block_functions.h"


BOOST_AUTO_TEST_CASE(create_)
{
  using namespace block;

  discr::Raster raster(3, 1, 1.0, 0.0, 0.0);
  discr::RasterData<REAL4> elevation(&raster);
  elevation.cell(0) = REAL4(1.0);
  pcr::setMV(elevation.cell(1));
  elevation.cell(2) = REAL4(3.0);

  boost::scoped_ptr<discr::Block> block(create(&elevation));
  BOOST_CHECK(block->cell(0).empty());
  BOOST_CHECK(dal::comparable(block->cell(0).baseElevation(), elevation.cell(0)));
  BOOST_CHECK(block->cell(1).empty());
  BOOST_CHECK(block->cell(1).isMV());
  BOOST_CHECK(block->cell(2).empty());
  BOOST_CHECK(dal::comparable(block->cell(2).baseElevation(), elevation.cell(2)));
}


BOOST_AUTO_TEST_CASE(base_elevation)
{
  using namespace block;

  typedef discr::Block::ThicknessType T;
  discr::Raster raster(3, 1);

  {
    discr::RasterData<REAL4> elevation(&raster, 3.0);
    pcr::setMV(elevation.cell(1));
    elevation.cell(2) = 2.2;
    discr::Block block(elevation);

    discr::RasterData<REAL4> result(&raster);
    baseElevation(result, block);


    BOOST_CHECK(!pcr::isMV(result.cell(0)));
    BOOST_CHECK(dal::comparable(result.cell(0), T(3.0)));
    BOOST_CHECK( pcr::isMV(result.cell(1)));
    BOOST_CHECK(!pcr::isMV(result.cell(2)));
    BOOST_CHECK(dal::comparable(result.cell(2), T(2.2)));
  }
}


BOOST_AUTO_TEST_CASE(surface_elevation)
{
  using namespace block;

  typedef discr::Block::ThicknessType T;
  discr::Raster raster(3, 1);

  {
    discr::RasterData<REAL4> elevation(&raster, 3.0);
    pcr::setMV(elevation.cell(1));
    elevation.cell(2) = 2.2;
    discr::Block block(elevation);

    discr::RasterData<REAL4> result(&raster);
    surfaceElevation(result, block);

    BOOST_CHECK(!pcr::isMV(result.cell(0)));
    BOOST_CHECK(dal::comparable(result.cell(0), T(3.0)));
    BOOST_CHECK( pcr::isMV(result.cell(1)));
    BOOST_CHECK(!pcr::isMV(result.cell(2)));
    BOOST_CHECK(dal::comparable(result.cell(2), T(2.2)));

    block.addVoxels(2, 5.0);

    surfaceElevation(result, block);
    BOOST_CHECK(!pcr::isMV(result.cell(0)));
    BOOST_CHECK(dal::comparable(result.cell(0), T(13.0)));
    BOOST_CHECK( pcr::isMV(result.cell(1)));
    BOOST_CHECK(!pcr::isMV(result.cell(2)));
    BOOST_CHECK(dal::comparable(result.cell(2), T(12.2)));
  }
}


BOOST_AUTO_TEST_CASE(set_default_value)
{
  using namespace block;

  // typedef discr::Block::ThicknessType T;
  discr::Raster raster(3, 1);

  {
    discr::RasterData<REAL4> elevation(&raster, 3.0);
    pcr::setMV(elevation.cell(1));
    elevation.cell(2) = 2.2;
    discr::Block block(elevation);

    discr::BlockData<INT4> sediment(&block);

    block.addVoxels(1, REAL4(2.3));
    BOOST_CHECK(!block.cell(0).isMV());
    BOOST_CHECK(sediment.cell(0).size() == 1);
    BOOST_CHECK(pcr::isMV(sediment.cell(0)[0]));

    BOOST_CHECK( block.cell(1).isMV());
    BOOST_CHECK(sediment.cell(1).empty());

    BOOST_CHECK(!block.cell(2).isMV());
    BOOST_CHECK(sediment.cell(2).size() == 1);
    BOOST_CHECK(pcr::isMV(sediment.cell(2)[0]));

    discr::RasterData<INT4> values(&raster);
    values.cell(0) = 5;
    values.cell(1) = 6;
    pcr::setMV(values.cell(2));

    setDefaultValue(sediment, values);

    block.addVoxels(1, REAL4(2.3));

    BOOST_CHECK(!block.cell(0).isMV());
    BOOST_CHECK(sediment.cell(0).size() == 2);
    BOOST_CHECK( pcr::isMV(sediment.cell(0)[0]));
    BOOST_CHECK(!pcr::isMV(sediment.cell(0)[1]));
    BOOST_CHECK(sediment.cell(0)[1] == 5);

    BOOST_CHECK( block.cell(1).isMV());
    BOOST_CHECK(sediment.cell(1).empty());

    BOOST_CHECK(!block.cell(2).isMV());
    BOOST_CHECK(sediment.cell(2).size() == 2);
    BOOST_CHECK( pcr::isMV(sediment.cell(2)[0]));
    BOOST_CHECK( pcr::isMV(sediment.cell(2)[1]));
  }
}
