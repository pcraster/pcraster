#define BOOST_TEST_MODULE pcraster dal use_cases
#include <boost/test/unit_test.hpp>
#include <boost/filesystem/operations.hpp>
#include "dev_GDalClient.h"
#include "dal_Exception.h"
#include "dal_Library.h"
#include "dal_RasterDal.h"
#include "dal_Client.h"

// static dal::Client client("/my/path/use_cases_test", true);


struct Fixture:
    private dev::GDalClient,
    private dal::Client

{

    Fixture()
        : dev::GDalClient(),
          dal::Client("/my/path/use_cases_test", true)
    {
    }

    ~Fixture()
    {
    }

};


BOOST_FIXTURE_TEST_SUITE(use_cases, Fixture)

BOOST_AUTO_TEST_CASE(test1)
{
  using namespace dal;

  dal::RasterDal rasterDal(true);

  // TODO verschil tussen "not existing" en "mallformed"/"unsupported format"

  // not existing
  boost::shared_ptr<dal::Raster> notExisting;
  boost::tie(notExisting, boost::tuples::ignore) = rasterDal.open(
      "notExisting.map");
  BOOST_CHECK(!notExisting);

  // mallformed
  boost::shared_ptr<dal::Raster> notRecognizedFormat;
  boost::tie(notRecognizedFormat, boost::tuples::ignore) = rasterDal.open(
      "main.cc");
  BOOST_CHECK(!notRecognizedFormat);

  boost::shared_ptr<dal::Raster> pcrMap;
  boost::tie(pcrMap, boost::tuples::ignore) = rasterDal.open("soil.map");
  BOOST_REQUIRE(pcrMap);
  BOOST_CHECK_EQUAL(pcrMap->typeId(), dal::TI_INT4);

  {
  boost::shared_ptr<dal::Raster> bilMap;
  boost::tie(bilMap, boost::tuples::ignore) = rasterDal.open("inp14_gl.bil");
  BOOST_REQUIRE(bilMap);
  BOOST_CHECK_EQUAL(bilMap->typeId(), dal::TI_REAL4);
  }

  // remainder assume avtutor dataset for Spatial Analyst existing
  boost::filesystem::path avtutor("c:/esri/av_gis30/avtutor/spatial");
  if (!boost::filesystem::exists(avtutor))
    return;

  // TODO NEXT move test from pcrgeo bil to here, make sure .stx is written
  // Test if we can open with QGIS?


  {
  boost::shared_ptr<dal::Raster> esriMap;
  boost::tie(esriMap, boost::tuples::ignore) = rasterDal.open(
                    "c:/esri/av_gis30/avtutor/spatial/dem");
  BOOST_CHECK(esriMap);
  BOOST_CHECK_EQUAL(esriMap->typeId(), dal::TI_REAL4);
  }

  {
  boost::shared_ptr<dal::Raster> esriMap;
  boost::tie(esriMap, boost::tuples::ignore) = rasterDal.open(
                   "c:/esri/av_gis30/avtutor/spatial/elevgrd");
  BOOST_CHECK(esriMap);
  BOOST_CHECK_EQUAL(esriMap->typeId(), dal::TI_REAL4);
  }

  {
  boost::shared_ptr<dal::Raster> esriMap;
  boost::tie(esriMap, boost::tuples::ignore) = rasterDal.open(
                   "c:/esri/av_gis30/avtutor/spatial/hillshd");
  BOOST_CHECK(esriMap);
  BOOST_CHECK_EQUAL(esriMap->typeId(), dal::TI_UINT1);
  bool arcCatalogSaysSignedIntegerPixelDept16=false;
  BOOST_WARN(arcCatalogSaysSignedIntegerPixelDept16);
  }
}


BOOST_AUTO_TEST_CASE(bil_format)
{
  using namespace dal;

  dal::RasterDal rasterDal(true);
  {
    // TEST SWAPPING
    // big endian, csf map (in diguise) with value 1 everywhere
    // but first column is MV, with maybe throw gdal in error
    boost::shared_ptr<dal::Raster> map;
    boost::tie(map, boost::tuples::ignore) = rasterDal.open("all1_float.bil");
    BOOST_REQUIRE(map);
    map = rasterDal.read("all1_float.bil", dal::TI_REAL4);
    BOOST_REQUIRE(map);

    BOOST_CHECK_EQUAL(map->typeId(), dal::TI_REAL4);
    BOOST_CHECK_EQUAL(map->nrRows(), size_t(4));
    BOOST_CHECK_EQUAL(map->nrCols(), size_t(4));

    REAL4 const* cells = map->cells<REAL4>();
    // PRINT_VAR(cells[0]); wil print nan OK!, no throw
    BOOST_CHECK_EQUAL(cells[1], 1.0f);
    BOOST_CHECK_EQUAL(cells[15], 1.0f);
  }
/*
*  { // create a  4*5 TI_INT2 map with first and last cell a MV of 0
*    com::write("NROWS 4\nNCOLS 5\nNBITS 16\nNODATA 0\n",
*    com::PathName("int2mv0.hdr"));
*    TI_INT2 buf[20];
*    std::generate_n(buf,20,com::SeqInc<TI_INT2>());
*    buf[19]=0;
*    com::write(buf,20*sizeof(TI_INT2),com::PathName("int2mv0.bil"));
*  }
*/

  {
   // dal::RasterDal rasterDal(true);
   boost::shared_ptr<dal::Raster> map;
   boost::tie(map, boost::tuples::ignore) = rasterDal.open("int2mv0.bil");
   BOOST_REQUIRE(map);
   BOOST_CHECK_EQUAL(map->typeId(), dal::TI_INT2);
  }

  { // a  4*5 TI_INT2 map with first and last cell a MV of 0
   //dal::RasterDal rasterDal(true);
   boost::shared_ptr<dal::Raster> map(rasterDal.read("int2mv0.bil",
      dal::TI_INT2));
   BOOST_REQUIRE(map);
   BOOST_CHECK_EQUAL(map->typeId(), dal::TI_INT2);
   BOOST_CHECK_EQUAL(map->cellSize(), 10);

   INT2 const* cells = map->cells<INT2>();
   BOOST_CHECK(pcr::isMV(cells[0]));
   BOOST_CHECK_EQUAL(cells[1], 1);
   BOOST_CHECK_EQUAL(cells[9], 9);
   BOOST_CHECK(pcr::isMV(cells[19]));
  }

/*
 * { // same and autoconver to INT4
 *  boost::shared_ptr<dal::Raster> map(rasterDal.read("int2mv0.bil",dal::TI_INT4));
 *  BOOST_CHECK(map);
 *  BOOST_CHECK_EQUAL(map->typeId(), dal::TI_INT4);
 *  BOOST_CHECK_EQUAL(map->cellSize(), 10);

 *  INT4 const* cells = map->cells<INT4>();
 *  BOOST_CHECK(pcr::isMV(cells[0]));
 *  BOOST_CHECK_EQUAL(cells[1], 1);
 *  BOOST_CHECK_EQUAL(cells[9], 9);
 *  BOOST_CHECK(pcr::isMV(cells[19]));
 * }
 */
}

/*
* //! setUp
*void geo::BandMapTest::setUp()
*{
*  { // create a minimal 4*5 TI_UINT1 map
*    com::write("NROWS 4\nNCOLS 5\n",
*    com::PathName("uint1minimal.hdr"));
*    TI_UINT1 buf[20];
*    std::generate_n(buf,20,com::SeqInc<TI_UINT1>());
*    com::write(buf,20,com::PathName("uint1minimal.bil"));
*  }
*  { // create a  4*5 TI_INT2 map with first and last cell a MV of 0
*    com::write("NROWS 4\nNCOLS 5\nNBITS 16\nNODATA 0\n",
*    com::PathName("int2mv0.hdr"));
*    TI_INT2 buf[20];
*    std::generate_n(buf,20,com::SeqInc<TI_INT2>());
*    buf[19]=0;
*    com::write(buf,20*sizeof(TI_INT2),com::PathName("int2mv0.bil"));
*  }
*}
*
*void geo::BandMapTest::testOpen()
*{
*  TI_UINT1 *buf=0;;
*  try {
*   BandMap bm("uint1minimal");
*   BOOST_CHECK_EQUAL(bm.nrRows(), size_t(4));
*   BOOST_CHECK_EQUAL(bm.nrCols(), size_t(5));
*   BOOST_CHECK_EQUAL(bm.nrCells(), 20);
*   BOOST_CHECK_EQUAL(size(com::PathName("uint1minimal.bil")), bm.nrCells());
*   buf = new TI_UINT1[bm.nrCells()];
*   BOOST_CHECK_EQUAL(bm.cellSize(), 1); // default
*   bm.getCellsAsUINT1(buf);
*   BOOST_CHECK(!bm.mvIsSet());
*   BOOST_CHECK_EQUAL(buf[12], 12);
*
*   createBil("uint1minimalcpy.bil",bm.rasterSpace(), buf);
*   BOOST_CHECK(com::filesExistsAndEqual("uint1minimalcpy.bil","uint1minimal.bil"));
*
*
*  } catch (const com::Exception& e) {
*     std::cerr << e.messages();
*  }
*  delete [] buf ;
*}
*
*
* todo reads and write and cmp default setting of ULYMAP and mapping that to RasterSpace
*void geo::BandMapTest::testRasterSpace()
*{
*}
*
*void geo::BandMapTest::testMultiBand()
*{
*  { // create a multiband 4*5 TI_REAL4 map
*    com::write("NROWS 4\nNCOLS 10\nNBANDS 3\nNBITS 32\n",
*               com::PathName("mband.hdr"));
*    TI_REAL4 buf[120];
*    std::generate_n(buf,120,com::SeqInc<TI_REAL4>());
*    com::write(buf,120*sizeof(TI_REAL4),com::PathName("mband.bil"));
*  }
* BandMap bm("mband");
* TI_REAL4 *REAL4 = new TI_REAL4[bm.nrCells()];
* bm.getCellsAsREAL4(REAL4);
* BOOST_CHECK_EQUAL(REAL4[0], 0);
* BOOST_CHECK_EQUAL(REAL4[10], 30);
* BOOST_CHECK_EQUAL(REAL4[20], 60);
* BOOST_CHECK_EQUAL(REAL4[39], 99);
* delete [] REAL4;
*}
*
*void geo::BandMapTest::testOpen2()
*{
* BandMap bm("int2mv0");
* BOOST_CHECK_EQUAL( bm.nrRows(), size_t(4));
* BOOST_CHECK_EQUAL( bm.nrCols(), size_t(5));
* BOOST_CHECK( bm.mvIsSet());
* BOOST_CHECK_EQUAL( bm.mvValue(), 0);
* BOOST_CHECK_EQUAL( bm.cellRepr(), CR_INT2);
*
* TI_INT2 *bufi2 = new TI_INT2[bm.nrCells()];
*
* bm.getCellsRaw(bufi2);
* BOOST_CHECK_EQUAL(bufi2[0], 0);
* BOOST_CHECK_EQUAL(bufi2[11], 11);
*
* TI_INT4 *bufi4 = new TI_INT4[bm.nrCells()];
* bm.getCellsAsINT4(bufi4);
* BOOST_CHECK_EQUAL(bufi4[19], MV_INT4);
* BOOST_CHECK_EQUAL(bufi4[0], MV_INT4);
* BOOST_CHECK_EQUAL(bufi4[11], 11);
*
* TI_REAL4 *REAL4 = new TI_REAL4[bm.nrCells()];
* bm.getCellsAsREAL4(REAL4);
* BOOST_CHECK(pcr::isMV(REAL4[0]));
* BOOST_CHECK_EQUAL(REAL4[11], 11.0);
*
* delete [] bufi4;
* delete [] bufi2;
* delete [] REAL4;
*}
*
*
*void geo::BandMapTest::testCreate()
*{
*   CSFMap  in("inp1b.map");
*   RasterSpace rs(in.rasterSpace());
*
*
*   TI_UINT1 *buf= new TI_UINT1[in.nrCells()];
*   in.getCells(buf);
*
*   createBil("inp1b",rs, buf,MV_UINT1);
*
*   BandMap  out("inp1b");
*
*   BOOST_CHECK_EQUAL(out.nrRows(), in.nrRows());
*   BOOST_CHECK_EQUAL(out.nrCols(), in.nrCols());
*   BOOST_CHECK_EQUAL(out.cellSize(), in.cellSize());
*   BOOST_CHECK_EQUAL(out.cellRepr(), in.cellRepr());
*
*   in.getCells(buf);
*   BOOST_CHECK_EQUAL(buf[0], MV_UINT1);
*   BOOST_CHECK_EQUAL(buf[1], 1);
*   BOOST_CHECK_EQUAL(buf[24], 1);
*
*   BOOST_CHECK_EQUAL(size(com::PathName("inp1b.bil")), in.nrCells());
*
*   BandMap asInt4("inp1b");
*   TI_INT4 *bufI4 = new TI_INT4[in.nrCells()];
*   asInt4.getCellsAsINT4(bufI4);
*   BOOST_CHECK_EQUAL(bufI4[0], MV_INT4);
*   BOOST_CHECK_EQUAL(bufI4[1], 1);
*   BOOST_CHECK_EQUAL(bufI4[24], 1);
*
*   delete [] bufI4;
*
*  delete [] buf ;
*}
*
*void geo::BandMapTest::testPutCells()
*{
* {
*  RasterSpace rs(4,5);
*  BandMap     bm("testPutCellsputINT4",rs,CR_INT4,false,0);
*  TI_INT4 createBuf[20];
*  std::generate_n(createBuf,20,com::SeqInc<TI_INT4>());
*  createBuf[0] =MV_INT4;
*  // test truncation/sign wrap due to TI_INT2 storage
*  createBuf[10]=INT2_MAX+1+99;
*
*  bm.putCellsAsINT4(createBuf);
*
*  BandMap     readBm("testPutCellsputINT4");
*  BOOST_CHECK_EQUAL(readBm.cellRepr(), CR_INT2);
*  BOOST_CHECK_EQUAL(readBm.nrRows(), size_t(4));
*  BOOST_CHECK_EQUAL(readBm.nrCols(), size_t(5));
*  TI_INT4 readBuf[20];
*  readBm.getCellsAsINT4(readBuf);
*  BOOST_CHECK_EQUAL(readBuf[0], MV_INT4);
*  BOOST_CHECK_EQUAL(readBuf[2], 2);
*  // test truncation/sign wrap due to TI_INT2 storage
*  BOOST_CHECK_EQUAL(readBuf[10], INT2_MIN-1+99);
* }
* {
*  RasterSpace rs(4,5);
*  BandMap     bm("testPutCellsputREAL4",rs,CR_REAL4,false,0);
*  TI_REAL4 createBuf[20];
*  std::generate_n(createBuf,20,com::SeqInc<TI_REAL4>());
*  pcr::setMV(createBuf[0]);
*
*  bm.putCellsAsREAL4(createBuf);
*
*  BandMap     readBm("testPutCellsputREAL4");
*  BOOST_CHECK_EQUAL(readBm.cellRepr(), CR_REAL4);
*  BOOST_CHECK_EQUAL(readBm.nrRows(), size_t(4));
*  BOOST_CHECK_EQUAL(readBm.nrCols(), size_t(5));
*  TI_REAL4 readBuf[20];
*
*  readBm.getCellsRaw(readBuf);
*  BOOST_CHECK_EQUAL(readBuf[0], -999);
*  BOOST_CHECK_EQUAL(readBuf[2], 2);
*
*  TI_REAL4 readBuf2[20];
*  readBm.getCellsAsREAL4(readBuf2);
*  BOOST_CHECK(pcr::isMV(readBuf2[0]));
*  BOOST_CHECK_EQUAL(readBuf2[2], 2);
*
*  // test stx
*  com::PathName stx("testPutCellsputREAL4.stx");
*  std::string stxContents;
*  com::read(stxContents,stx);
*  BOOST_CHECK_EQUAL(stxContents, "1 1 19\n");
* }
*}
*
*void geo::BandMapTest::testHeader()
*{
*  TI_UINT1 buf[20];
*  std::generate_n(buf,20,com::SeqInc<TI_UINT1>());
*  com::write(buf,20,com::PathName("headertest.bil"));
*  com::PathName pn("headertest.hdr");
*
*  // comments are allowed
*  // unknown keys are discarded
*  bool succes=true;
*  try {
*    com::write("NROWS 4 A COMMENT\nNCOLS 5\nUNKNOWNKEY 5\n",pn);
*    BandMap bm("headertest");
*  } catch(...) {
*    succes=false;
*  }
*  BOOST_CHECK(succes);
*
*  // NROWS and NCOLS are required
*  bool failure=false;
*  try {
*    com::write("XDIM 1\nUNKNOWNKEY 5\n",pn);
*    BandMap bm("headertest");
*  } catch(const com::Exception& e) {
*    BOOST_CHECK(e.messages().find("NROWS") != std::string::npos
*           || e.messages().find("NCOLS") != std::string::npos);
*    failure=true;
*  }
*  BOOST_CHECK(failure);
*
*  // allow ommision of last new line
*  succes=true;
*  try {
*    com::write("NROWS 4\nNCOLS 5",pn);
*    BandMap bm("headertest");
*  } catch(...) {
*    succes=false;
*  }
*  BOOST_CHECK(succes);
*
*  {
*    // if both dim are set, then ok
*    com::write("NROWS 4\nNCOLS 5\nXDIM 0.5\nYDIM 0.5", pn);
*    BandMap bm("headertest");
*    BOOST_CHECK_EQUAL(bm.cellSize(), 0.5); // as set
*  }
*
*  {
*    // if only one dim then the default is set
*    com::write("NROWS 4\nNCOLS 5\nXDIM 0.5\n", pn);
*    BandMap bm("headertest");
*    BOOST_CHECK_EQUAL(bm.cellSize(), 1); // the default
*  }
*
*  // if both DIM set they must be equal
*  failure=false;
*  try {
*    com::write("NROWS 3\nNCOLS 4\nXDIM 1\nYDIM 0.5\n",pn);
*    BandMap bm("headertest");
*  } catch(const com::Exception& e) {
*    BOOST_CHECK(e.messages().find("XDIM") != std::string::npos
*           && e.messages().find("YDIM") != std::string::npos);
*    failure=true;
*  }
*  BOOST_CHECK(failure);
*}
*
*/

BOOST_AUTO_TEST_SUITE_END()
