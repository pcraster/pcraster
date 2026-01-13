#define BOOST_TEST_MODULE pcraster com file_map
#include <boost/test/unit_test.hpp>
#include "com_filemap.h"
#include "com_pathname.h"
#include "com_strlib.h"
#include "com_file.h"
#include "com_math.h"
#include "com_exception.h"

BOOST_AUTO_TEST_CASE(empty_file)
{
  com::PathName const pn("empty.filemap");
  com::create(pn);
#ifdef WIN32
  com::FileMap fm(pn);
  BOOST_TEST(fm.begin() == fm.pointer());
  BOOST_TEST(fm.begin() == fm.end());
#else
  // linux no mmap call with length 0 allowed since 2.6.something
  bool catched = false;
  try {
    com::FileMap const fm(pn);
  } catch (const com::OpenFileError &e) {
    catched = true;
    BOOST_TEST(e.messages().find("mmap does not support 0 sized files") != std::string::npos);
  }
  BOOST_TEST(catched);
#endif
}

BOOST_AUTO_TEST_CASE(iterators)
{

  // read some stuff
  const char *files[2] = {"zinc.unix.eas", "zinc.dos.eas"};
  std::vector<std::string> header;
  header.push_back("Zinc measurements on River Meuse flood plains");
  header.push_back("3");
  header.push_back("xcoord, m");
  header.push_back("ycoord, m");
  header.push_back("zinc, ppm");
  header.push_back("181072 333611 1022");

  for (auto &file : files) {
    com::PathName const pn(file);
    com::FileMap const fm(pn);
    std::string const contents(fm.begin(), fm.end());
    std::vector<std::string> lines(com::split(contents, '\n'));
    BOOST_TEST(lines.size() == 5 + 155);
    for (size_t l = 0; l < header.size(); l++) {
      if (lines[l][0] == '\r') {
        lines[l].erase(0, 1);
      }
      if (!lines[l].empty()) {
        size_t const last = lines[l].size() - 1;
        if (lines[l][last] == '\r') {
          lines[l].erase(last, 1);
        }
      }
      BOOST_TEST(header[l] == lines[l]);
    }
  }
}

BOOST_AUTO_TEST_CASE(file_map_to_large)
{
  using namespace com;

#ifdef WIN32
  com::PathName big("E:\\gam_allXL.xyz");
#else
  com::PathName const big("/home/cees/tmp/gam_allXL.xyz");
#endif
  if (com::exists(big)) {
    BOOST_TEST(size(big) > gigaByte<size_t>(2));
    testOpenForReading(big);
    bool catched(false);
    try {
      FileMap const n(big);
    } catch (const com::OpenFileError &e) {
      catched = true;

      BOOST_TEST(e.messages().find("Too large to map in memory") != std::string::npos);
    }
    BOOST_TEST(catched);
  }
}
