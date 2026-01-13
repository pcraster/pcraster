#define BOOST_TEST_MODULE pcraster geo band_map
#include <boost/test/unit_test.hpp>
#include "geo_bandmap.h"
#include "com_file.h"
#include "geo_csfmap.h"
#include "com_csfcell.h"
#include "geo_rasterspace.h"
#include "com_exception.h"
#include "com_algorithm.h"

struct Fixture {

  Fixture()
  {
    {  // create a minimal 4*5 UINT1 map
      com::write("NROWS 4\nNCOLS 5\n", com::PathName("uint1minimal.hdr"));
      UINT1 buf[20];
      std::generate_n(buf, 20, com::SeqInc<UINT1>());
      com::write(buf, 20, com::PathName("uint1minimal.bil"));
    }
    {  // create a  4*5 INT2 map with first and last cell a MV of 0
      // above 8 bits always set byteorder!
      com::write("NROWS 4\nNCOLS 5\nNBITS 16\nNODATA 0\nBYTEORDER I", com::PathName("int2mv0.hdr"));
      INT2 buf[20];
      std::generate_n(buf, 20, com::SeqInc<INT2>());
      buf[19] = 0;
      com::write(buf, 20 * sizeof(INT2), com::PathName("int2mv0.bil"));
    }
  }

  ~Fixture()
  {
  }
};


BOOST_FIXTURE_TEST_SUITE(band_map, Fixture)

// TODO make test to
//    - reads data from a file not big enough


BOOST_AUTO_TEST_CASE(open)
{
  using namespace geo;

  UINT1 *buf = nullptr;
  ;
  try {
    BandMap const bm("uint1minimal");
    BOOST_TEST(bm.nrRows() == 4);
    BOOST_TEST(bm.nrCols() == 5);
    BOOST_TEST(bm.nrCells() == 20);
    BOOST_TEST(size(com::PathName("uint1minimal.bil")) == bm.nrCells());
    buf = new UINT1[bm.nrCells()];
    BOOST_TEST(bm.cellSize() == 1);  // default
    bm.getCellsAsUINT1(buf);
    BOOST_TEST(!bm.mvIsSet());
    BOOST_TEST(buf[12] == 12);

    createBil("uint1minimalcpy.bil", bm.rasterSpace(), buf);
    BOOST_TEST(com::filesExistsAndEqual("uint1minimalcpy.bil", "uint1minimal.bil"));


  } catch (const com::Exception &e) {
    std::cerr << e.messages();
  }
  delete[] buf;
}

// reads and write and cmp default setting of ULYMAP and mapping that to RasterSpace
BOOST_AUTO_TEST_CASE(raster_space)
{
}

BOOST_AUTO_TEST_CASE(multi_band)
{
  using namespace geo;

  {  // create a multiband 4*5 REAL4 map
    com::write("NROWS 4\nNCOLS 10\nNBANDS 3\nNBITS 32\n", com::PathName("mband.hdr"));
    REAL4 buf[120];
    std::generate_n(buf, 120, com::SeqInc<REAL4>());
    com::write(buf, 120 * sizeof(REAL4), com::PathName("mband.bil"));
  }
  BandMap const bm("mband");
  auto *real4 = new REAL4[bm.nrCells()];
  bm.getCellsAsREAL4(real4);
  BOOST_TEST(real4[0] == 0);
  BOOST_TEST(real4[10] == 30);
  BOOST_TEST(real4[20] == 60);
  BOOST_TEST(real4[39] == 99);
  delete[] real4;
}

BOOST_AUTO_TEST_CASE(open2)
{
  using namespace geo;

  BandMap const bm("int2mv0");
  BOOST_TEST(bm.nrRows() == 4);
  BOOST_TEST(bm.nrCols() == 5);
  BOOST_TEST(bm.mvIsSet());
  BOOST_TEST(bm.mvValue() == 0);
  BOOST_TEST(bm.cellRepr() == CR_INT2);

  INT2 *bufi2 = new INT2[bm.nrCells()];

  bm.getCellsRaw(bufi2);
  BOOST_TEST(bufi2[0] == 0);
  BOOST_TEST(bufi2[11] == 11);

  INT4 *bufi4 = new INT4[bm.nrCells()];
  bm.getCellsAsINT4(bufi4);
  BOOST_TEST(bufi4[19] == MV_INT4);
  BOOST_TEST(bufi4[0] == MV_INT4);
  BOOST_TEST(bufi4[11] == 11);

  auto *real4 = new REAL4[bm.nrCells()];
  bm.getCellsAsREAL4(real4);
  BOOST_TEST(pcr::isMV(real4[0]));
  BOOST_TEST(real4[11] == 11.0);

  delete[] bufi4;
  delete[] bufi2;
  delete[] real4;
}

BOOST_AUTO_TEST_CASE(read)
{
  using namespace geo;

  // TEST SWAPPING
  // big endian, csf map with value
  // 1 everywhere
  // but first column is MV
  BandMap const bm("all1_float.bil");
  BOOST_TEST(bm.nrRows() == 4);
  BOOST_TEST(bm.nrCols() == 4);
  auto *buf = new REAL4[bm.nrCells()];
  BOOST_TEST(bm.cellSize() == 1);

  bm.getCellsRaw(buf);
  BOOST_TEST(buf[1] == 1);
  BOOST_TEST(buf[bm.nrCells() - 1] == 1);

  delete[] buf;
}

BOOST_AUTO_TEST_CASE(create)
{
  using namespace geo;

  CSFMap in("inp1b.map");
  RasterSpace const rs(in.rasterSpace());


  auto *buf = new UINT1[in.nrCells()];
  in.getCells(buf);

  createBil("inp1b", rs, buf, MV_UINT1);

  BandMap const out("inp1b");

  BOOST_TEST(out.nrRows() == in.nrRows());
  BOOST_TEST(out.nrCols() == in.nrCols());
  BOOST_TEST(out.cellSize() == in.cellSize());
  BOOST_TEST(out.cellRepr() == in.cellRepr());

  in.getCells(buf);
  BOOST_TEST(buf[0] == MV_UINT1);
  BOOST_TEST(buf[1] == 1);
  BOOST_TEST(buf[24] == 1);

  BOOST_TEST(size(com::PathName("inp1b.bil")) == in.nrCells());

  BandMap const asInt4("inp1b");
  INT4 *bufI4 = new INT4[in.nrCells()];
  asInt4.getCellsAsINT4(bufI4);
  BOOST_TEST(bufI4[0] == MV_INT4);
  BOOST_TEST(bufI4[1] == 1);
  BOOST_TEST(bufI4[24] == 1);

  delete[] bufI4;

  delete[] buf;
}

BOOST_AUTO_TEST_CASE(put_cells)
{
  using namespace geo;

  {
    RasterSpace const rs(4, 5);
    BandMap const bm("testPutCellsputINT4", rs, CR_INT4, false, 0);
    INT4 createBuf[20];
    std::generate_n(createBuf, 20, com::SeqInc<INT4>());
    createBuf[0] = MV_INT4;
    // test truncation/sign wrap due to INT2 storage
    createBuf[10] = INT2_MAX + 1 + 99;

    bm.putCellsAsINT4(createBuf);

    BandMap const readBm("testPutCellsputINT4");
    BOOST_TEST(readBm.cellRepr() == CR_INT2);
    BOOST_TEST(readBm.nrRows() == 4);
    BOOST_TEST(readBm.nrCols() == 5);
    INT4 readBuf[20];
    readBm.getCellsAsINT4(readBuf);
    BOOST_TEST(readBuf[0] == MV_INT4);
    BOOST_TEST(readBuf[2] == 2);
    // test truncation/sign wrap due to INT2 storage
    BOOST_TEST(readBuf[10] == INT2_MIN - 1 + 99);
  }
  {
    RasterSpace const rs(4, 5);
    BandMap const bm("testPutCellsputREAL4", rs, CR_REAL4, false, 0);
    REAL4 createBuf[20];
    std::generate_n(createBuf, 20, com::SeqInc<REAL4>());
    pcr::setMV(createBuf[0]);

    bm.putCellsAsREAL4(createBuf);

    BandMap const readBm("testPutCellsputREAL4");
    BOOST_TEST(readBm.cellRepr() == CR_REAL4);
    BOOST_TEST(readBm.nrRows() == 4);
    BOOST_TEST(readBm.nrCols() == 5);
    REAL4 readBuf[20];

    readBm.getCellsRaw(readBuf);
    BOOST_TEST(readBuf[0] == -999);
    BOOST_TEST(readBuf[2] == 2);

    REAL4 readBuf2[20];
    readBm.getCellsAsREAL4(readBuf2);
    BOOST_TEST(pcr::isMV(readBuf2[0]));
    BOOST_TEST(readBuf2[2] == 2);

    // test stx
    com::PathName const stx("testPutCellsputREAL4.stx");
    std::string stxContents;
    com::read(stxContents, stx);
    BOOST_TEST(stxContents == "1 1 19\n");
  }
}

BOOST_AUTO_TEST_CASE(header)
{
  using namespace geo;

  UINT1 buf[20];
  std::generate_n(buf, 20, com::SeqInc<UINT1>());
  com::write(buf, 20, com::PathName("headertest.bil"));
  com::PathName const pn("headertest.hdr");

  // comments are allowed
  // unknown keys are discarded
  bool succes = true;
  try {
    com::write("NROWS 4 A COMMENT\nNCOLS 5\nUNKNOWNKEY 5\n", pn);
    BandMap const bm("headertest");
  } catch (...) {
    succes = false;
  }
  BOOST_TEST(succes);

  // NROWS and NCOLS are required
  bool failure = false;
  try {
    com::write("XDIM 1\nUNKNOWNKEY 5\n", pn);
    BandMap const bm("headertest");
  } catch (const com::Exception &e) {
    BOOST_TEST((e.messages().find("NROWS") != std::string::npos ||
                e.messages().find("NCOLS") != std::string::npos));
    failure = true;
  }
  BOOST_TEST(failure);

  // allow ommision of last new line
  succes = true;
  try {
    com::write("NROWS 4\nNCOLS 5", pn);
    BandMap const bm("headertest");
  } catch (...) {
    succes = false;
  }
  BOOST_TEST(succes);

  {
    // if both dim are set, then ok
    com::write("NROWS 4\nNCOLS 5\nXDIM 0.5\nYDIM 0.5", pn);
    BandMap const bm("headertest");
    BOOST_TEST(bm.cellSize() == 0.5);  // as set
  }

  {
    // if only one dim then the default is set
    com::write("NROWS 4\nNCOLS 5\nXDIM 0.5\n", pn);
    BandMap const bm("headertest");
    BOOST_TEST(bm.cellSize() == 1);  // the default
  }

  // if both DIM set they must be equal
  failure = false;
  try {
    com::write("NROWS 3\nNCOLS 4\nXDIM 1\nYDIM 0.5\n", pn);
    BandMap const bm("headertest");
  } catch (const com::Exception &e) {
    BOOST_TEST((e.messages().find("XDIM") != std::string::npos &&
                e.messages().find("YDIM") != std::string::npos));
    failure = true;
  }
  BOOST_TEST(failure);
}

BOOST_AUTO_TEST_SUITE_END()
