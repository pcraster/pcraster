#define BOOST_TEST_MODULE com test suite
#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_PCRDLL
#include "pcrdll.h"
#define INCLUDED_PCRDLL
#endif
#ifndef INCLUDED_BOOST_TEST_INCLUDED_UNIT_TEST
#include <boost/test/included/unit_test.hpp>
#define INCLUDED_BOOST_TEST_INCLUDED_UNIT_TEST
#endif

#ifndef INCLUDED_COM_ARGUMENTPARSERTEST
#include "com_argumentparsertest.h"
#define INCLUDED_COM_ARGUMENTPARSERTEST
#endif

#ifndef INCLUDED_COM_ANYPOINTERSTEST
#include "com_anypointerstest.h"
#define INCLUDED_COM_ANYPOINTERSTEST
#endif

#ifndef INCLUDED_COM_ALGORITHMTEST
#include "com_algorithmtest.h"
#define INCLUDED_COM_ALGORITHMTEST
#endif

#ifndef INCLUDED_COM_APPARGSTEST
#include "com_appargstest.h"
#define INCLUDED_COM_APPARGSTEST
#endif

#ifndef INCLUDED_COM_BINARYOPERATORSTEST
#include "com_binaryoperatorstest.h"
#define INCLUDED_COM_BINARYOPERATORSTEST
#endif

#ifndef INCLUDED_COM_COMMANDLINETEST
#include "com_commandlinetest.h"
#define INCLUDED_COM_COMMANDLINETEST
#endif

#ifndef INCLUDED_COM_COMMANDLINEARGUMENTTEST
#include "com_commandlineargumenttest.h"
#define INCLUDED_COM_COMMANDLINEARGUMENTTEST
#endif

#ifndef INCLUDED_COM_COMMANDLINEARGUMENTSTEST
#include "com_commandlineargumentstest.h"
#define INCLUDED_COM_COMMANDLINEARGUMENTSTEST
#endif

#ifndef INCLUDED_COM_CPUCYCLECOUNTERTEST
#include "com_cpucyclecountertest.h"
#define INCLUDED_COM_CPUCYCLECOUNTERTEST
#endif

#ifndef INCLUDED_COM_CSFCELLTEST
#include "com_csfcelltest.h"
#define INCLUDED_COM_CSFCELLTEST
#endif

#ifndef INCLUDED_COM_DIRECTORYTEST
#include "com_directorytest.h"
#define INCLUDED_COM_DIRECTORYTEST
#endif

#ifndef INCLUDED_COM_FILETEST
#include "com_filetest.h"
#define INCLUDED_COM_FILETEST
#endif

#ifndef INCLUDED_COM_FUNCTIONSTEST
#include "com_functionstest.h"
#define INCLUDED_COM_FUNCTIONSTEST
#endif

#ifndef INCLUDED_COM_LABELEDPROGRESSTRACKERTEST
#include "com_labeledprogresstrackertest.h"
#define INCLUDED_COM_LABELEDPROGRESSTRACKERTEST
#endif

#ifndef INCLUDED_COM_MVOPTEST
#include "com_mvoptest.h"
#define INCLUDED_COM_MVOPTEST
#endif

#ifndef INCLUDED_COM_PARSERSTEST
#include "com_parserstest.h"
#define INCLUDED_COM_PARSERSTEST
#endif

#ifndef INCLUDED_COM_PATHINFOTEST
#include "com_pathinfotest.h"
#define INCLUDED_COM_PATHINFOTEST
#endif

#ifndef INCLUDED_COM_PATHNAMETEST
#include "com_pathnametest.h"
#define INCLUDED_COM_PATHNAMETEST
#endif

#ifndef INCLUDED_COM_PROGRESSBARTEST
#include "com_progressbartest.h"
#define INCLUDED_COM_PROGRESSBARTEST
#endif

#ifndef INCLUDED_COM_UNORDEREDCROSSTABLETEST
#include "com_unorderedcrosstabletest.h"
#define INCLUDED_COM_UNORDEREDCROSSTABLETEST
#endif

#ifndef INCLUDED_COM_STRLIBTEST
#include "com_strlibtest.h"
#define INCLUDED_COM_STRLIBTEST
#endif

#ifndef INCLUDED_COM_INTERVALTEST
#include "com_intervaltest.h"
#define INCLUDED_COM_INTERVALTEST
#endif

#ifndef INCLUDED_COM_STATISTICSTEST
#include "com_statisticstest.h"
#define INCLUDED_COM_STATISTICSTEST
#endif

#ifndef INCLUDED_COM_MATHTEST
#include "com_mathtest.h"
#define INCLUDED_COM_MATHTEST
#endif

#ifndef INCLUDED_COM_KEYVALUETABLETEST
#include "com_keyvaluetabletest.h"
#define INCLUDED_COM_KEYVALUETABLETEST
#endif

#ifdef WIN32
# ifndef INCLUDED_COM_WIN32REGISTRYKEYTEST
#  include "com_win32registrykeytest.h"
# define INCLUDED_COM_WIN32REGISTRYKEYTEST
# endif
#endif
#ifndef INCLUDED_COM_SINGLEVALUEDRASTERTEST
#include "com_singlevaluedrastertest.h"
#define INCLUDED_COM_SINGLEVALUEDRASTERTEST
#endif
#ifndef INCLUDED_COM_RASTERTEST
#include "com_rastertest.h"
#define INCLUDED_COM_RASTERTEST
#endif

#ifndef INCLUDED_COM_IRASTERTEST
#include "com_irastertest.h"
#define INCLUDED_COM_IRASTERTEST
#endif

#ifndef INCLUDED_COM_RLEPTRVECTORTEST
#include "com_rleptrvectortest.h"
#define INCLUDED_COM_RLEPTRVECTORTEST
#endif
#ifndef INCLUDED_COM_TABLETEST
#include "com_tabletest.h"
#define INCLUDED_COM_TABLETEST
#endif
#ifndef INCLUDED_COM_TABLEINFOTEST
#include "com_tableinfotest.h"
#define INCLUDED_COM_TABLEINFOTEST
#endif
#ifndef INCLUDED_COM_INTABLESTREAMTEST
#include "com_intablestreamtest.h"
#define INCLUDED_COM_INTABLESTREAMTEST
#endif
#ifndef INCLUDED_COM_FILEMAPTEST
#include "com_filemaptest.h"
#define INCLUDED_COM_FILEMAPTEST
#endif
#ifndef INCLUDED_COM_SPIRITFILEPARSERTEST
#include "com_spiritfileparsertest.h"
#define INCLUDED_COM_SPIRITFILEPARSERTEST
#endif
#ifndef INCLUDED_COM_SPIRITFILELINEPARSERTEST
#include "com_spiritfilelineparsertest.h"
#define INCLUDED_COM_SPIRITFILELINEPARSERTEST
#endif
#ifndef INCLUDED_COM_INTERVALMAPTEST
#include "com_intervalmaptest.h"
#define INCLUDED_COM_INTERVALMAPTEST
#endif
#ifndef INCLUDED_COM_MVGENERICTEST
#include "com_mvgenerictest.h"
#define INCLUDED_COM_MVGENERICTEST
#endif

#ifndef INCLUDED_COM_CLONETEST
#include "com_clonetest.h"
#define INCLUDED_COM_CLONETEST
#endif

#ifndef INCLUDED_COM_SPAWNTEST
#include "com_spawntest.h"
#define INCLUDED_COM_SPAWNTEST
#endif

#ifndef INCLUDED_COM_TEMPDIRECTORYTEST
#include "com_tempdirectorytest.h"
#define INCLUDED_COM_TEMPDIRECTORYTEST
#endif


boost::unit_test::test_suite* init_unit_test_suite(int /* argc */, char ** const /*argv*/)
{
  boost::unit_test::test_suite* test = BOOST_TEST_SUITE(__FILE__);
  test->add(com::LabeledProgressTrackerTest().suite());
  test->add(com::ProgressBarTest().suite());
#ifdef WIN32
  test->add(com::Win32RegistryKeyTest().suite());
#endif
  test->add(com::MVOpTest().suite());
  // remove see Bugzilla 246 test->add(com::SpawnTest().suite());
  test->add(com::AnyPointersTest().suite());
  test->add(com::IntervalTest().suite());
  test->add(com::StatisticsTest().suite());

  test->add(com::IntervalMapTest().suite());
  test->add(com::MVGenericTest().suite());

  test->add(com::FunctionsTest().suite());
  test->add(com::BinaryOperatorsTest().suite());
  test->add(com::ParsersTest().suite());
  test->add(com::SingleValuedRasterTest().suite());
  test->add(com::RasterTest().suite());
  test->add(com::IRasterTest().suite());
  test->add(com::KeyValueTableTest().suite());
  test->add(com::AlgorithmTest().suite());
  test->add(com::CSFCellTest().suite());
  test->add(com::UnOrderedCrossTableTest().suite());
  test->add(com::DirectoryTest().suite());
  // test->add(com::ArgumentParserTest().suite());
  // test->add(com::AppArgsTest().suite());
  // test->add(com::CommandLineArgumentTest().suite());
  // test->add(com::CommandLineArgumentsTest().suite());
  // test->add(com::CommandLineTest().suite());
  test->add(com::FileTest().suite());
  test->add(com::RLEPtrVectorTest().suite());
  test->add(com::FileMapTest().suite());
  test->add(com::StrLibTest().suite());
  test->add(com::MathTest().suite());
  test->add(com::TableTest().suite());
  test->add(com::SpiritFileParserTest().suite());
  test->add(com::PathNameTest().suite());
  test->add(com::PathInfoTest().suite());
  test->add(com::TempDirectoryTest().suite());

  // deprecated, see InTableStream docs
  // test->add(com::TableInfoTest().suite());
  // test->add(com::SpiritFileLineParserTest().suite());
  // test->add(com::InTableStreamTest().suite());

  test->add(com::CloneTest().suite());
  test->add(com::CpuCycleCounterTest().suite());

  return test;

}


