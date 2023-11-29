add_library(pcraster_old_calc_objects OBJECT
    calc_stattable.cc
    calc_repeatuntil.cc
    calc_valuebuffer.cc
    calc_compressor.cc
    calc_maskcompressor.cc
    calc_nullcompressor.cc
    calc_compressioninput.cc
    calc_decompresseddata.cc
    calc_globargs.cc
    calc_globresult.cc
    calc_globarg.cc
    calc_spatial.cc
    calc_nonspatial.cc
    calc_field.cc
    calc_gridmap.cc
    calc_zeromap.cc
    calc_iocsffieldstrategy.cc
    calc_ioesrifieldstrategy.cc
    calc_iofieldstrategy.cc
    calc_iobandfieldstrategy.cc
    calc_bandmap.cc
    calc_branchexpr.cc
    calc_branchexprimpl.cc
    calc_constant.cc
    calc_constantnumbertype.cc
    calc_csfmap.cc
    calc_doubleass.cc
    calc_esrimap.cc
    calc_esrigridio.cc
    calc_liberror.cc
    calc_generatespatialfunc.cc
    calc_quote.cc
    calc_rundirectory.cc
    calc_stackreader.cc
    calc_manualexampletester.cc
    calc_positionname.cc
    calc_lexinput.cc
    calc_lexinputsource.cc
    calc_extsym.cc
    calc_lexinputsourcetextscript.cc
    calc_lexinputsourcestring.cc
    calc_position.cc
    calc_positiontext.cc
    calc_positionnone.cc
    calc_modelbuilder.cc
    calc_wldelfthabitat.cc
    calc_reporttable.cc
    calc_bindingtable.cc
    calc_parserinput.cc
    calc_modelparser.cc
    calc_element.cc
    calc_quitforexitoption.cc
    calc_quitforprogresscallback.cc
    calc_quitpremature.cc
    calc_progresscallback.cc
    calc_clientinterface.cc
    calc_iclientinterface.cc
    calc_runsettings.cc
    calc_calc.cc
    calc_dllcalc.cc
    calc_script.cc
    calc_iscript.cc
    calc_apimap.cc
    calc_arraydefinition.cc
    calc_arraydefvector.cc
    calc_assignment.cc
    calc_bindedsymbol.cc
    calc_blockposition.cc
    calc_dynamicsection.cc
    calc_fieldargs.cc
    calc_fieldexpr.cc
    calc_fieldleaf.cc
    calc_fieldleft.cc
    calc_fieldmapinputparameter.cc
    calc_fieldnewparameter.cc
    calc_fieldnrparameter.cc
    calc_fieldparameter.cc
    calc_fieldstack.cc
    calc_fieldtype.cc
    calc_fieldvalue.cc
    calc_file.cc
    calc_filewriter.cc
    calc_foreach.cc
    calc_idlist.cc
    calc_indexcontainer.cc
    calc_indexparameter.cc
    calc_indexparameterconstant.cc
    calc_indexparametervariable.cc
    calc_indexselectedsingle.cc
    calc_indexselectedvector.cc
    calc_indexset.cc
    calc_indextable.cc
    calc_infoscript.cc
    calc_innerstatementblock.cc
    calc_lookupexpr.cc
    calc_cmdlinecalc.cc
    calc_methodoperator.cc
    calc_mixedexpr.cc
    calc_modellinkmethodexpr.cc
    calc_modellinkinit.cc
    calc_modellinkmethodstatement.cc
    calc_nonspatialimpl.cc
    calc_nonspatialtssimpl.cc
    calc_objcount.cc
    calc_operator.cc
    calc_parameter.cc
    calc_parsindex.cc
    calc_parspar.cc
    calc_posexception.cc
    calc_report.cc
    calc_spatialimpl.cc
    calc_stackcr.cc
    calc_stackinput.cc
    calc_statement.cc
    calc_statementblock.cc
    calc_stdoutstat.cc
    calc_subparameter.cc
    calc_symbol.cc
    calc_lookuptableleaf.cc
    calc_lookuptableparameter.cc
    calc_lookuptable.cc
    calc_lookuplinear.cc
    calc_timeinputexpr.cc
    calc_timeoutput.cc
    calc_tssinputparameter.cc
    calc_timetable.cc
    calc_tssinputleaf.cc
    calc_tssoutputparameter.cc
    calc_tssoutputvalue.cc
    calc_tssparameter.cc
    calc_usedefnode.cc
    calc_usermodellink.cc
    calc_usersymbol.cc
    calc_vs.cc
    calc_writeinfo.cc
    calc_timervalue.cc
    calc_symboltable.cc
    calc_defpar.cc
    calc_usepar.cc
    calc_map2csf.cc
    lexvaluefile.cc
    lexnewcalc.cc
    Parser.cc
    calcparser.cc
    calc_externalsymbols.cc
    calc_findsymbol.cc
    pcrcalc.cc
    calc_arcviewextapi.cc
    dassfunc.c
    dbinfunc.c
    dunfunc.c
    genfunc.c
    globfunc.c
    sbinfunc.c
    sunfunc.c
)


set_source_files_properties(dbinfunc.c
    PROPERTIES
        COMPILE_OPTIONS
            "$<$<CXX_COMPILER_ID:GNU>:-Wno-misleading-indentation>"
)

set_source_files_properties(calcparser.cc
    PROPERTIES
        COMPILE_OPTIONS
            "$<$<COMPILE_LANG_AND_ID:CXX,GNU,AppleClang,Clang>:-Wno-all;-Wno-error>"
)

target_include_directories(pcraster_old_calc_objects
    PRIVATE
        $<TARGET_PROPERTY:pcraster::calc,INTERFACE_INCLUDE_DIRECTORIES>
        $<TARGET_PROPERTY:pcraster::pcrxml,INTERFACE_INCLUDE_DIRECTORIES>
        $<TARGET_PROPERTY:pcraster::tab,INTERFACE_INCLUDE_DIRECTORIES>
)

# We do not care much about warnings when using ...
target_include_directories(pcraster_old_calc_objects SYSTEM
    PRIVATE
        $<TARGET_PROPERTY:pcraster::pccts,INTERFACE_INCLUDE_DIRECTORIES>
        ${Boost_INCLUDE_DIRS}
)

target_include_directories(pcraster_old_calc_objects
    PUBLIC
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
)

# Add flags required for Qt 6
if ((MSVC) AND (MSVC_VERSION GREATER_EQUAL 1914))
    target_compile_options(pcraster_old_calc_objects PRIVATE "/Zc:__cplusplus" "/permissive-")
endif()

target_compile_definitions(pcraster_old_calc_objects
    PRIVATE
        USE_IN_PCRCALC
        YY_NO_UNISTD_H
        YY_NEVER_INTERACTIVE
        PCR_DAL_SHARED_LINK
)

add_library(pcraster_old_calc SHARED
    $<TARGET_OBJECTS:pcraster_old_calc_objects>
)

add_library(pcraster::old_calc ALIAS pcraster_old_calc)

set_target_properties(pcraster_old_calc
    PROPERTIES
        SOVERSION ${${PROJECT_NAME}_MAJOR_VERSION}
        VERSION ${${PROJECT_NAME}_MAJOR_VERSION}.${${PROJECT_NAME}_MINOR_VERSION}.${${PROJECT_NAME}_PATCH_VERSION}
)

target_include_directories(pcraster_old_calc
    PUBLIC
        $<TARGET_PROPERTY:pcraster_old_calc_objects,INTERFACE_INCLUDE_DIRECTORIES>
)

target_link_libraries(pcraster_old_calc
    PUBLIC
        pcraster::calc
        pcraster::pccts
        pcraster::pcrxml
        pcraster::tab
)

install(
    TARGETS
        pcraster_old_calc
    RUNTIME
        DESTINATION ${PCRASTER_BIN_INSTALL_DIR} COMPONENT Runtime
    LIBRARY
        DESTINATION ${PCRASTER_LIB_INSTALL_DIR} COMPONENT Runtime
)

if(PCRASTER_BUILD_TEST)
    configure_file(
        test_prolog.py.in
        test_prolog.py
        @ONLY
    )

    add_unit_tests(
        SCOPE oldcalc
        NAMES
            calc_tssoutputvaluetest.cc
            calc_maskcompressortest.cc
            calc_manualexampletestertest.cc
            calc_manualexamplestest.cc
            calc_nonspatialtest.cc
            calc_spatialtest.cc
            calc_lookuptabletest.cc
            calc_lookuplineartest.cc
            # calc_clientinterfacetest.cc  No longer supported.
            calc_rundirectorytest.cc
            calc_iocsffieldstrategytest.cc
            # calc_iobandfieldstrategytest.cc  No longer supported.
            # calc_iobandtest.cc  No longer supported.
            calc_parsertest.cc
            calc_lexinputtest.cc
            calc_modelbuildertest.cc
            # calc_wldelfthabitattest.cc  No longer supported.
        OBJECT_LIBRARIES
            $<TARGET_OBJECTS:pcraster_old_calc_objects>
        LINK_LIBRARIES
            $<TARGET_PROPERTY:pcraster_old_calc,LINK_LIBRARIES>
    )
    add_test(
        NAME setup_oldcalc
        COMMAND ${Python3_EXECUTABLE} test_prolog.py
    )

    set_tests_properties(setup_oldcalc
        PROPERTIES
            FIXTURES_SETUP
                FixturePcrOldCalc
    )

    set_tests_properties(oldcalc_calc_tssoutputvaluetest
        PROPERTIES
            FIXTURES_REQUIRED
                FixturePcrOldCalc
    )
endif()


# /*! \page design Design
#  *
#  * <H1>Operation classification</H1>
#  * <p>
#  * Classification of operation is needed to implement transformations of the code to execute,
#  * such as MV compression. compiled point-sub models and ldd transformations and code executing
#  * on formats such as vectors or tables. Also if a (series of) statements has a particular classification.
#  * These statements may be used yes or no in other situations such as <i>cellfocus</i>.
#  * The latter leads to the definition of the term <B>computing unit</B>: a point where the result
#  * of computation must be stored: a raster cell, a polygon (vector) a record (table).
#  * <B>(data) container</B>: a collection,  a set of computing units. <b>Check OMI definitions</b>
#  * <table cols="2">
#  *  <tr>
#  *   <td>collection</td><td>computing unit</td><td>length/area</td>
#  *  </tr>
#  *  <tr>
#  *   <td>raster</td><td>cell</td><td>cellsize</td>
#  *  </tr>
#  *  <tr>
#  *   <td>vector</td><td>polygon (single polygon)</td><td>polygon area</td>
#  *  </tr>
#  *  <tr>
#  *   <td>table</td><td>record (no geo referencing)</td><td>1</td>
#  *  </tr>
#  *  <tr>
#  *   <td>points</td><td>point (data with geo referencing)</td><td>area=0, 1 for counting</td>
#  *  </tr>
#  * </table>
#  * </p>
#  *
#  * <p>
#  * classification 1: operation decomposition, is it decomposable and how?
#  * <dl>
#  *  <dt>point</dt>
#  *  <dd>(a*b)+2 is computable in many ways, per computing unit or a*b for whole collection and then
#  *  add 2 for whole collection. Only the inputs of the same computing unit are needed.</dd>
#  *  <dt>global</dt>
#  *    <dd>the operation needs the whole collection.</dd>
#  *       <dl>
#  *        <dt>unordered</dt>
#  *         <dd>the operation can access the collection in any order, (.e.g areaaverage, maptotal).</dd>
#  *        <dt>ordered</dt>
#  *         <dd>the operation must access the collection in a particular order. Order are defined a priori to
#  *         the operation (ldd order in accuflux) or set by the input data collections (spread).
#  *         Further sub classification possible here are the type of iterators: accuflux, forward iterator
#  *         start at the end nodes, spread/clump random iterator.
#  *         <ol>
#  *          <li>neighbour topology (clump, spread)</li>
#  *          <li>ldd topology  (accuflux, dynamicwave), kan op 2 manieren, beginnen bij pit of bij punten zonder instroom</li>
#  *        </ol>
#  *         </dd>
#  * </dl>
#  * </p>
#  *
#  * <p>
#  * classification 2: context information needed. A collection is internally just some data structure (array,
#  * list) storing the computing unit. Some operation however need context, such as location, area (support) of
#  * the computing unit. Context information seems to be a list of items, the operation needs yes or no.
#  * <dl>
#  *  <dt>area:</dt><dd>the area (oppervlakte) of each computing unit is part of the operation, areaarea,
#  *      cellarea</dd>
#  *  <dt>length:</dt><dd>the length of a computing unit is part of the operation, ldddist.</dd>
#  *  <dt>location:</dt><dd>the location of a computing unit is part of the operation, xcoordinate, move.</dd>
#  *  <dt>random:</dt><dd>uses the random generator</dd>
#  * </dl>
#  * Operaties die items gebruiken, staan in de manual met --cellunit (area,length), --coorul (location)
#  * enzo.
#  * For all types of collections a value for these items can be chosen, they are not all sensible but will
#  * create a running system. For example, points have a location, choosing the value 1 for area and length, will
#  * make areaarea a sort of point counting operation, and ldddist a non sensable operation.
#  *
#  * Legends doorerven (sub-classing, Nominal, ordinal, boolean)
#  * <H1>Andere punten</H1>
#  * Tag kaarten om te kijken of ze runtime veranderen: date stamping
#  * </p>
#   <pre>
#   iterators:
#     - whole map
#     - voor een locatie de 4 of 8 buurcellen, incl/excl cell zelf. zie ook Kor's filter code
#     - voor een locatie met ldd, de upstream of de downstream cellen.
#   ldd transformatie: cellen in 1d-array zodat bovenstroomse cellen altijd
#    een lager index hebben dan hun beneden stroomse (zie rflow executable)
#   - alle andere kaarten zouden ook in deze volgorde kunnen.
#   - Is de optimale volgorde  als de afstand in de 1d-array tussen de door de
#     ldd verbonden cellen zo klein mogelijk is (> cache hits)?
#
#   comp. sub point models:
#    - genereer C(++) code, die wordt gecompileerd naar een DDL die runtime
#      wordt gebonden, zoals ik dat ook reeds doe met de gridio.dll en de
#      modellink dll's.
#      Target
#        - linux: gcc
#        - win32: gcc-mingw probably (of de gratis borland C++ compiler)
#   </pre>
#
#
#  */
#
# /*!
#  \namespace calc
#  \brief The pcrcalc application
#
#  \todo
#   <p>DLL REFACTORING
#    <pre>
#
# get rid of  static in any of the following modules:
#  - calc_arcviewextapi.cc
#         static char *errStr(0);
#
# clients of pcrcalcl.dll:
# C++  - pcrcalc.exe
# C++  - calcui.exe       (ClientInterface)
# C    - arcView extension
# C    - DelftShell
#
# Member calc::EsriGridIO::EsriGridIO()
#  make a difference in fail between - lib not found, is ArcView installed?
#   - sym not found, do we have correct version of av-dll
#
#
# Member calc::IoEsriFieldStrategy::setStackInfo(const StackInfo &s) const
#    update delphi-DLL for new PCRaster DTD.  (CHECK OF AL GEDAAN)
#    do dataType in directoryStackInfo
#    assure that float read correct in Delphi "," or "." issue
#
# REVIEW again what classes must be exported (PCR_DLL_EXP/PCRAPI)
#    </pre>
#     END OF DLL REFACTORING
#     </p>
#
#  \todo
#   2) strip the .lib of pcrcalcl.dll for bare essentials
#
#  \todo
#    when model terminates because of numerical error, -e or whatever
#    does it write all timeseries, adjust maps/min/max's etc..?
#    It seems it does, put that behavior in  tests
#
#  \todo
#    optimizations:
#    <pre>
#     MV test:
#      (!(v+1)  : UINT* REAL*
#      (!(v<<1) : INT*
#     Lees AMD Opt guide:
#       - prefeching
#       - elk field heeft:
#         - een bitmap voor MV's
#           die bitmap is geshared indien hetzelfde
#           vanuit de bitmap, per 4 bit-strip de mogelijkheden bekijken, voor
#            maximal pipelining.
#    </pre>
#
#  \bug
#    Check "dynmod\\plants\\scen.mod"
#     als naam is newplants (9 chars)
#      dan result zonder punt, dus newplants001
#
#   \bug
#      pcrcalc/test230, tan(180), solution tan(pi()/2:--radians) ???
#
#  \bug
#    lookup...() met arg 1 as a field, geeft assertion failed
#
#  <H1>Binding issues</H1>
#    - calcui, maakt binding.ipcr aan
#      met daarin NumericSetting
#    - als -r in een pad een directory AANMAAKT dan check parent voor
#       een binding.ipcr waarin een child moet worden toegevoegd!
#    - zet ModelRunStatus/@ResultLastRun voor begin run op Cancel
#       dan:
#        - bij completion op Done
#        - bij error op Error (eigenlijk Incomplete als input not found)
#    - voeg in MRO manual toe, dat binding.ipcr wordt gebruikt.
#    - ModelRunStatus in binding.ipcr is ook zinnig buiten calcui
#
#    Externe bindings
#      - bindings in volgorde van definitie presenteren (wil Ad ook)
#      - externe bindings mogen overschrijven/dubbel zijn (test375)
#      - de externe bindings moet ook reeds in het script voorkomen
#        PROBLEEM, calcui doet niet alleen de bindings maar kan nieuwe
#         bindings generern die niet in de binding staan.
#      - maak opties:
#         - -b set external bindings
#         - om binding.ipcr te generen ?
#         - -R as -r but tells how run is/will be done:
#            - where input files are found
#            - numerical values
#         - diffs of two scenarios
#
#
#    rundirectories/freebie: -i inputDir -o outputDir
#     d.m.v. het rundirectory mechanisme, geen zoekpad ALLEEN die directories
#     gebruiken.
#
#   <H1>Binding</H1>
#    NumericSetting,FileSetting --> BindingRewriter moet in pcrxml-library
#
#   refactor non-working symbolSequenceNr()
#
#   Refactor Pos:
#    - Pos kan ook naar een binding.ipcr wijzen?
#       --> yep, reimplement posError for a bindedSymbol!
#
# !  bij refactoren van pcrcalc, als Pos meer flexibel is dan foutmelding altijd
# !  op de externe binding, die is voor de gebruiker zichtbaar
#
#    - context in ModelBuilder kan ook xml file zijn
#      posError is een rare naam
#
#    <H1>Parameter bescrijvingen</H1>
#    Twee interval definities:
#     allow-interval: set on input, give assertion if computation goes outside
#     adjust-interval: set to this interval, if computation goes outside
# */
