#include "stddefx.h"

/*! \page design Design
 *
 * <H1>Operation classification</H1>
 * <p>
 * Classification of operation is needed to implement transformations of the code to execute,
 * such as MV compression. compiled point-sub models and ldd transformations and code executing
 * on formats such as vectors or tables. Also if a (series of) statements has a particular classification.
 * These statements may be used yes or no in other situations such as <i>cellfocus</i>.
 * The latter leads to the definition of the term <B>computing unit</B>: a point where the result
 * of computation must be stored: a raster cell, a polygon (vector) a record (table).
 * <B>(data) container</B>: a collection,  a set of computing units. <b>Check OMI definitions</b>
 * <table cols="2">
 *  <tr>
 *   <td>collection</td><td>computing unit</td><td>length/area</td>
 *  </tr>
 *  <tr>
 *   <td>raster</td><td>cell</td><td>cellsize</td>
 *  </tr>
 *  <tr>
 *   <td>vector</td><td>polygon (single polygon)</td><td>polygon area</td>
 *  </tr>
 *  <tr>
 *   <td>table</td><td>record (no geo referencing)</td><td>1</td>
 *  </tr>
 *  <tr>
 *   <td>points</td><td>point (data with geo referencing)</td><td>area=0, 1 for counting</td>
 *  </tr>
 * </table>
 * </p>
 *
 * <p>
 * classification 1: operation decomposition, is it decomposable and how?
 * <dl>
 *  <dt>point</dt>
 *  <dd>(a*b)+2 is computable in many ways, per computing unit or a*b for whole collection and then
 *  add 2 for whole collection. Only the inputs of the same computing unit are needed.</dd>
 *  <dt>global</dt>
 *    <dd>the operation needs the whole collection.</dd>
 *       <dl>
 *        <dt>unordered</dt>
 *         <dd>the operation can access the collection in any order, (.e.g areaaverage, maptotal).</dd>
 *        <dt>ordered</dt>
 *         <dd>the operation must access the collection in a particular order. Order are defined a priori to
 *         the operation (ldd order in accuflux) or set by the input data collections (spread).
 *         Further sub classification possible here are the type of iterators: accuflux, forward iterator
 *         start at the end nodes, spread/clump random iterator.
 *         <ol>
 *          <li>neighbour topology (clump, spread)</li>
 *          <li>ldd topology  (accuflux, dynamicwave), kan op 2 manieren, beginnen bij pit of bij punten zonder instroom</li>
 *        </ol>
 *         </dd>
 * </dl>
 * </p>
 *
 * <p>
 * classification 2: context information needed. A collection is internally just some data structure (array,
 * list) storing the computing unit. Some operation however need context, such as location, area (support) of
 * the computing unit. Context information seems to be a list of items, the operation needs yes or no.
 * <dl>
 *  <dt>area:</dt><dd>the area (oppervlakte) of each computing unit is part of the operation, areaarea,
 *      cellarea</dd>
 *  <dt>length:</dt><dd>the length of a computing unit is part of the operation, ldddist.</dd>
 *  <dt>location:</dt><dd>the location of a computing unit is part of the operation, xcoordinate, move.</dd>
 *  <dt>random:</dt><dd>uses the random generator</dd>
 * </dl>
 * Operaties die items gebruiken, staan in de manual met --cellunit (area,length), --coorul (location) 
 * enzo.
 * For all types of collections a value for these items can be chosen, they are not all sensible but will
 * create a running system. For example, points have a location, choosing the value 1 for area and length, will
 * make areaarea a sort of point counting operation, and ldddist a non sensable operation.
 *
 * Legends doorerven (sub-classing, Nominal, ordinal, boolean)
 * <H1>Andere punten</H1>
 * Tag kaarten om te kijken of ze runtime veranderen: date stamping
 * </p>
  <pre>
  iterators:
    - whole map
    - voor een locatie de 4 of 8 buurcellen, incl/excl cell zelf. zie ook Kor's filter code
    - voor een locatie met ldd, de upstream of de downstream cellen.
  ldd transformatie: cellen in 1d-array zodat bovenstroomse cellen altijd
   een lager index hebben dan hun beneden stroomse (zie rflow executable)
  - alle andere kaarten zouden ook in deze volgorde kunnen.
  - Is de optimale volgorde  als de afstand in de 1d-array tussen de door de
    ldd verbonden cellen zo klein mogelijk is (> cache hits)?

  comp. sub point models:
   - genereer C(++) code, die wordt gecompileerd naar een DDL die runtime
     wordt gebonden, zoals ik dat ook reeds doe met de gridio.dll en de
     modellink dll's.
     Target
       - linux: gcc
       - win32: gcc-mingw probably (of de gratis borland C++ compiler)
  </pre>


 */

/*!
 \namespace calc
 \brief The pcrcalc application

 \todo
  <p>DLL REFACTORING
   <pre>

get rid of  static in any of the following modules:
 - calc_arcviewextapi.cc
        static char *errStr(0);

clients of pcrcalcl.dll:
C++  - pcrcalc.exe
C++  - calcui.exe       (ClientInterface)
C    - arcView extension
C    - DelftShell

Member calc::EsriGridIO::EsriGridIO()
 make a difference in fail between - lib not found, is ArcView installed? 
  - sym not found, do we have correct version of av-dll 


Member calc::IoEsriFieldStrategy::setStackInfo(const StackInfo &s) const 
   update delphi-DLL for new PCRaster DTD.  (CHECK OF AL GEDAAN)
   do dataType in directoryStackInfo 
   assure that float read correct in Delphi "," or "." issue

REVIEW again what classes must be exported (PCR_DLL_EXP/PCRAPI)
   </pre>
    END OF DLL REFACTORING
    </p>

 \todo
  2) strip the .lib of pcrcalcl.dll for bare essentials

 \todo
   when model terminates because of numerical error, -e or whatever
   does it write all timeseries, adjust maps/min/max's etc..?
   It seems it does, put that behavior in  tests

 \todo
   optimizations:
   <pre>
    MV test:
     (!(v+1)  : UINT* REAL*
     (!(v<<1) : INT*
    Lees AMD Opt guide:
      - prefeching
      - elk field heeft:
        - een bitmap voor MV's
          die bitmap is geshared indien hetzelfde
          vanuit de bitmap, per 4 bit-strip de mogelijkheden bekijken, voor
           maximal pipelining.
   </pre>

 \bug
   Check "dynmod\\plants\\scen.mod"
    als naam is newplants (9 chars)
     dan result zonder punt, dus newplants001

  \bug
     pcrcalc/test230, tan(180), solution tan(pi()/2:--radians) ???

 \bug
   lookup...() met arg 1 as a field, geeft assertion failed

 <H1>Binding issues</H1>
   - calcui, maakt binding.ipcr aan
     met daarin NumericSetting
   - als -r in een pad een directory AANMAAKT dan check parent voor
      een binding.ipcr waarin een child moet worden toegevoegd!
   - zet ModelRunStatus/@ResultLastRun voor begin run op Cancel
      dan:
       - bij completion op Done
       - bij error op Error (eigenlijk Incomplete als input not found)
   - voeg in MRO manual toe, dat binding.ipcr wordt gebruikt.
   - ModelRunStatus in binding.ipcr is ook zinnig buiten calcui

   Externe bindings
     - bindings in volgorde van definitie presenteren (wil Ad ook)
     - externe bindings mogen overschrijven/dubbel zijn (test375)
     - de externe bindings moet ook reeds in het script voorkomen
       PROBLEEM, calcui doet niet alleen de bindings maar kan nieuwe
        bindings generern die niet in de binding staan.
     - maak opties:
        - -b set external bindings
        - om binding.ipcr te generen ?
        - -R as -r but tells how run is/will be done:
           - where input files are found
           - numerical values
        - diffs of two scenarios


   rundirectories/freebie: -i inputDir -o outputDir
    d.m.v. het rundirectory mechanisme, geen zoekpad ALLEEN die directories
    gebruiken.

  <H1>Binding</H1>
   NumericSetting,FileSetting --> BindingRewriter moet in pcrxml-library

  refactor non-working symbolSequenceNr()

  Refactor Pos:
   - Pos kan ook naar een binding.ipcr wijzen?
      --> yep, reimplement posError for a bindedSymbol!

!  bij refactoren van pcrcalc, als Pos meer flexibel is dan foutmelding altijd
!  op de externe binding, die is voor de gebruiker zichtbaar

   - context in ModelBuilder kan ook xml file zijn
     posError is een rare naam

   <H1>Parameter bescrijvingen</H1>
   Twee interval definities:
    allow-interval: set on input, give assertion if computation goes outside
    adjust-interval: set to this interval, if computation goes outside
*/


#ifndef INCLUDED_BOOST_TEST_INCLUDED_UNIT_TEST
#include <boost/test/included/unit_test.hpp>
#define INCLUDED_BOOST_TEST_INCLUDED_UNIT_TEST
#endif

#ifndef INCLUDED_CALC_RUNDIRECTORYTEST
#include "calc_rundirectorytest.h"
#define INCLUDED_CALC_RUNDIRECTORYTEST
#endif
#ifndef INCLUDED_CALC_IOCSFFIELDSTRATEGYTEST
#include "calc_iocsffieldstrategytest.h"
#define INCLUDED_CALC_IOCSFFIELDSTRATEGYTEST
#endif
#ifndef INCLUDED_CALC_CLIENTINTERFACETEST
#include "calc_clientinterfacetest.h"
#define INCLUDED_CALC_CLIENTINTERFACETEST
#endif
#ifndef INCLUDED_CALC_IOBANDFIELDSTRATEGYTEST
#include "calc_iobandfieldstrategytest.h"
#define INCLUDED_CALC_IOBANDFIELDSTRATEGYTEST
#endif
#ifndef INCLUDED_CALC_IOBANDTEST
#include "calc_iobandtest.h"
#define INCLUDED_CALC_IOBANDTEST
#endif

#ifndef INCLUDED_CALC_PARSERTEST
#include "calc_parsertest.h"
#define INCLUDED_CALC_PARSERTEST
#endif

#ifndef INCLUDED_CALC_LEXINPUTTEST
#include "calc_lexinputtest.h"
#define INCLUDED_CALC_LEXINPUTTEST
#endif

#ifndef INCLUDED_CALC_MODELBUILDERTEST
#include "calc_modelbuildertest.h"
#define INCLUDED_CALC_MODELBUILDERTEST
#endif
#ifndef INCLUDED_CALC_LOOKUPTABLETEST
#include "calc_lookuptabletest.h"
#define INCLUDED_CALC_LOOKUPTABLETEST
#endif
#ifndef INCLUDED_CALC_LOOKUPLINEARTEST
#include "calc_lookuplineartest.h"
#define INCLUDED_CALC_LOOKUPLINEARTEST
#endif
#ifndef INCLUDED_CALC_CALC
#include "calc_calc.h"  // globalInit()
#define INCLUDED_CALC_CALC
#endif
#ifndef INCLUDED_CALC_NONSPATIALTEST
#include "calc_nonspatialtest.h"
#define INCLUDED_CALC_NONSPATIALTEST
#endif
#ifndef INCLUDED_CALC_SPATIALTEST
#include "calc_spatialtest.h"
#define INCLUDED_CALC_SPATIALTEST
#endif
#ifndef INCLUDED_CALC_WLDELFTHABITATTEST
#include "calc_wldelfthabitattest.h"
#define INCLUDED_CALC_WLDELFTHABITATTEST
#endif
#ifndef INCLUDED_CALC_MANUALEXAMPLETESTERTEST
#include "calc_manualexampletestertest.h"
#define INCLUDED_CALC_MANUALEXAMPLETESTERTEST
#endif
#ifndef INCLUDED_CALC_MANUALEXAMPLESTEST
#include "calc_manualexamplestest.h"
#define INCLUDED_CALC_MANUALEXAMPLESTEST
#endif
#ifndef INCLUDED_CALC_MASKCOMPRESSORTEST
#include "calc_maskcompressortest.h"
#define INCLUDED_CALC_MASKCOMPRESSORTEST
#endif
#ifndef INCLUDED_CALC_TSSOUTPUTVALUETEST
#include "calc_tssoutputvaluetest.h"
#define INCLUDED_CALC_TSSOUTPUTVALUETEST
#endif


 boost::unit_test::test_suite* init_unit_test_suite(int /*argc*/, char ** const /*argv*/) {

  calc::globalInit();

  boost::unit_test::test_suite* test = BOOST_TEST_SUITE(__FILE__);

  test->add(calc::TssOutputValueTest().suite());
  test->add(calc::NonSpatialTest().suite());
  test->add(calc::SpatialTest().suite());
  test->add(calc::MaskCompressorTest().suite());

  test->add(calc::LookupTableTest().suite());
  test->add(calc::LookupLinearTest().suite());
  test->add(calc::ModelBuilderTest().suite());
  test->add(calc::ParserTest().suite());

  test->add(calc::LexInputTest().suite());

  // NO LONGER sUPPORTED test->add(calc::ClientInterfaceTest().suite());
  test->add(calc::RunDirectoryTest().suite());
  test->add(calc::IoCsfFieldStrategyTest().suite());
  // NO LONGER SUPPORTED test->add(calc::IoBandFieldStrategyTest().suite());
  test->add(calc::ManualExampleTesterTest().suite());

  test->add(calc::ManualExamplesTest().suite());


  // must be last, since global option appIoStrategy is changed
  // NO LONGER SUPPORTED test->add(calc::IoBandTest().suite());

  // NO LONGER SUPPORTED test->add(calc::WlDelftHabitatTest().suite());

  return test;
 }

