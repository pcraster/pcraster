#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_COM_INTABLESTREAMTEST
#include "com_intablestreamtest.h"
#define INCLUDED_COM_INTABLESTREAMTEST
#endif

// Library headers.
#ifndef INCLUDED_BOOST_TEST_TEST_TOOLS
#include <boost/test/test_tools.hpp>
#define INCLUDED_BOOST_TEST_TEST_TOOLS
#endif

#ifndef INCLUDED_BOOST_TEST_UNIT_TEST_SUITE
#include <boost/test/unit_test_suite.hpp>
#define INCLUDED_BOOST_TEST_UNIT_TEST_SUITE
#endif

// PCRaster library headers.
#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif

// Module headers.
#ifndef INCLUDED_COM_INTABLESTREAM
#include "com_intablestream.h"
#define INCLUDED_COM_INTABLESTREAM
#endif
#ifndef INCLUDED_COM_PATHNAME
#include "com_pathname.h"
#define INCLUDED_COM_PATHNAME
#endif
#ifndef INCLUDED_COM_FILE
#include "com_file.h"
#define INCLUDED_COM_FILE
#endif

#include <memory>

/*!
  \file
  This file contains the implementation of the InTableStreamTest class.
*/


//------------------------------------------------------------------------------
// DEFINITION OF STATIC INTABLESTREAM MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite *com::InTableStreamTest::suite()
{
  boost::unit_test::test_suite *suite = BOOST_TEST_SUITE(__FILE__);
  std::shared_ptr<InTableStreamTest> instance(new InTableStreamTest());

  suite->add(BOOST_CLASS_TEST_CASE(&InTableStreamTest::testRead, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&InTableStreamTest::testRead2, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&InTableStreamTest::testFormatErrors, instance));

  return suite;
}

//------------------------------------------------------------------------------
// DEFINITION OF INTABLESTREAM MEMBERS
//------------------------------------------------------------------------------

//! ctor
com::InTableStreamTest::InTableStreamTest()
{
}

//! setUp
void com::InTableStreamTest::setUp()
{
}

//! tearDown
void com::InTableStreamTest::tearDown()
{
}

void com::InTableStreamTest::testRead()
{
  const char *files[2] = {"zinc.unix.eas", "zinc.dos.eas"};
  for (size_t i = 0; i < 2; i++) {
    InTableStream tab(files[i]);
    BOOST_TEST(tab.layout() == InTableStream::GEO_EAS);
    std::vector<double> d;
    tab >> d;
    BOOST_TEST(d.size() == 3);
    BOOST_TEST(d[0] == 181072);
    BOOST_TEST(d[1] == 333611);
    BOOST_TEST(d[2] == 1022);
  }

  PathName pn("inTableStream.txt");
  std::vector<double> d;

  const char *testSimples[3] = {//simple and some extra new lines
                                "1\n2\n \n3\n",
                                // no new line at end
                                "1\n2\n3", "1\n2\n3 \n 4"};
  for (size_t i = 0; i < 3; i++) {
    write(testSimples[i], pn);
    InTableStream tab(pn);
    BOOST_TEST(tab.layout() == InTableStream::PLAIN_TXT);
    size_t v = 1;
    while (tab >> d) {
      BOOST_TEST(d.size() == 1);
      BOOST_TEST(d[0] == v++);
    }
    BOOST_TEST(i < 2 ? v == 4 : v == 5);
  }
  {
    write("\n2\n\n0", pn);
    InTableStream tab(pn);
    BOOST_TEST(tab.lineNrStartLastRead() == 0);
    BOOST_TEST(tab.layout() == InTableStream::PLAIN_TXT);
    BOOST_TEST(tab.nrColumns() == 1);
    BOOST_TEST(tab >> d);
    BOOST_TEST(tab.lineNrStartLastRead() == 2);
    BOOST_TEST(d.size() == 1 && d[0] == 2);
    BOOST_TEST(tab >> d);
    BOOST_TEST(tab.lineNrStartLastRead() == 4);
    BOOST_TEST(d.size() == 1 && d[0] == 0);
    BOOST_TEST(!(tab >> d));
  }
  {  // geo eas with no data
    write("\n2\n\ntitle2 ", pn);
    InTableStream tab(pn);
    BOOST_TEST(tab.layout() == InTableStream::GEO_EAS);
    BOOST_TEST(tab.nrColumns() == 2);
    BOOST_TEST(tab.columnNames()[0].empty());
    BOOST_TEST(tab.columnNames()[1] == "title2");
    BOOST_TEST(!(tab >> d));
    BOOST_TEST(d.empty());
  }
  {  // geo eas with numeric descriptions and a single line of data
    write("1\n2\n0\n0\n2 3", pn);
    InTableStream tab(pn);
    BOOST_TEST(tab.layout() == InTableStream::GEO_EAS);
    BOOST_TEST(tab.nrColumns() == 2);
    BOOST_TEST(tab.columnNames()[0] == "0");
    BOOST_TEST(tab.columnNames()[1] == "0");
    BOOST_TEST(tab >> d);
    BOOST_TEST(tab.lineNrStartLastRead() == 5);
    BOOST_TEST(d.size() == 2 && d[0] == 2 && d[1] == 3);
    BOOST_TEST(!(tab >> d));
  }
}

void com::InTableStreamTest::testRead2()
{
  PathName pn("inTableStream2.txt");
  std::vector<double> d;
  {  // text with single line of data
    write("0", pn);
    InTableStream tab(pn);
    BOOST_TEST(tab.layout() == InTableStream::PLAIN_TXT);
    BOOST_TEST(tab.nrColumns() == 1);
    BOOST_TEST(tab.columnNames()[0].empty());
    BOOST_TEST(tab >> d);
    BOOST_TEST(tab.lineNrStartLastRead() == 1);
    BOOST_TEST(d.size() == 1 && d[0] == 0);
    BOOST_TEST(!(tab >> d));
  }
  {  // text with 2 lines of data but irregular
    write("0\n1 2", pn);
    InTableStream tab(pn);
    BOOST_TEST(tab.layout() == InTableStream::PLAIN_TXT);
    BOOST_TEST(tab >> d);
    BOOST_TEST(d.size() == 1 && d[0] == 0);
    BOOST_TEST(tab >> d);
    BOOST_TEST(d.size() == 2 && d[0] == 1 && d[1] == 2);
    BOOST_TEST(!(tab >> d));
  }
  {  // file with only space
    write(" \t \n  ", pn);
    InTableStream tab(pn);
    BOOST_TEST(tab.layout() == InTableStream::EMPTY);
    BOOST_TEST(tab.nrColumns() == 0);
    BOOST_TEST(!(tab >> d));
    // pretty useles and incorrect:
    // BOOST_TEST(tab.lineNrStartLastRead()==1);
    BOOST_TEST(d.empty());
  }
  {  // empty file, 0 bytes
    write("", pn);
    BOOST_TEST(size(pn) == 0);
    InTableStream tab(pn);
    BOOST_TEST(tab.layout() == InTableStream::EMPTY);
    BOOST_TEST(tab.nrColumns() == 0);
    BOOST_TEST(!(tab >> d));
    BOOST_TEST(d.empty());
  }
  {  // geoEas with incorrect data (1 record too long)
    com::write("geoEas d\n2\nt1\nt2\n1 2 3\n4 5 \n", pn);
    InTableStream tab(pn);
    BOOST_TEST(tab.layout() == TableInfo::GEO_EAS);
    BOOST_TEST(tab.nrColumns() == 2);
    BOOST_TEST(tab >> d);
    BOOST_TEST(d.size() == 3);
    BOOST_TEST(tab >> d);
    BOOST_TEST(d.size() == 2);
    BOOST_TEST(!(tab >> d));
  }
}

void com::InTableStreamTest::testFormatErrors()
{
  {  // not a table file
    PathName pn("spawnScript");
    bool catched(false);
    try {
      InTableStream tab(pn);
    } catch (const FileFormatError &e) {

      BOOST_TEST(e.messages().find("Format of file") != std::string::npos);
      catched = true;
    }
    BOOST_TEST(catched);
  }
  PathName pn("inTableStream.txt");

  {  // geo eas but short on column names
    write("description\n3\ntitle1\ntitle2\n", pn);
    bool catched(false);
    try {
      InTableStream tab(pn);
    } catch (const FileFormatError &e) {
      BOOST_TEST(e.messages().find("Format of file") != std::string::npos);
      catched = true;
    }
    BOOST_TEST(catched);
  }

  {  // illegal format
    write("1\n2\n3\n4\n ac\n3\n", pn);
    InTableStream tab(pn);
    BOOST_TEST(tab.layout() == InTableStream::PLAIN_TXT);
    std::vector<double> d;
    bool catched(false);
    try {
      while (tab >> d)
        ;
    } catch (const FilePositionError &e) {
      BOOST_TEST(e.messages().find("ac") != std::string::npos);
      BOOST_TEST(e.lineNr() == 5);
      BOOST_TEST(e.columnNr() == 2);
      catched = true;
    }
    BOOST_TEST(catched);
  }
}
