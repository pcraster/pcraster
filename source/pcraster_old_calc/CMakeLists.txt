add_library(pcraster_old_calc_objects OBJECT
    calc_stattable
    calc_repeatuntil
    calc_valuebuffer
    calc_compressor
    calc_maskcompressor
    calc_nullcompressor
    calc_compressioninput
    calc_decompresseddata
    calc_globargs
    calc_globresult
    calc_globarg
    calc_spatial
    calc_nonspatial
    calc_field
    calc_gridmap
    calc_zeromap
    calc_iocsffieldstrategy
    calc_ioesrifieldstrategy
    calc_iofieldstrategy
    calc_iobandfieldstrategy
    calc_bandmap
    calc_branchexpr
    calc_branchexprimpl
    calc_constant
    calc_constantnumbertype
    calc_csfmap
    calc_doubleass
    calc_esrimap
    calc_esrigridio
    calc_liberror
    calc_generatespatialfunc
    calc_quote
    calc_rundirectory
    calc_stackreader
    calc_manualexampletester
    calc_positionname
    calc_lexinput
    calc_lexinputsource
    calc_extsym
    calc_lexinputsourcetextscript
    calc_lexinputsourcestring
    calc_position
    calc_positiontext
    calc_positionnone
    calc_modelbuilder
    calc_wldelfthabitat
    calc_reporttable
    calc_bindingtable
    calc_parserinput
    calc_modelparser
    calc_element
    calc_quitforexitoption
    calc_quitforprogresscallback
    calc_quitpremature
    calc_progresscallback
    calc_clientinterface
    calc_iclientinterface
    calc_runsettings
    calc_calc
    calc_dllcalc
    calc_script
    calc_iscript
    calc_apimap
    calc_arraydefinition
    calc_arraydefvector
    calc_assignment
    calc_bindedsymbol
    calc_blockposition
    calc_dynamicsection
    calc_fieldargs
    calc_fieldexpr
    calc_fieldleaf
    calc_fieldleft
    calc_fieldmapinputparameter
    calc_fieldnewparameter
    calc_fieldnrparameter
    calc_fieldparameter
    calc_fieldstack
    calc_fieldtype
    calc_fieldvalue
    calc_file
    calc_filewriter
    calc_foreach
    calc_idlist
    calc_indexcontainer
    calc_indexparameter
    calc_indexparameterconstant
    calc_indexparametervariable
    calc_indexselectedsingle
    calc_indexselectedvector
    calc_indexset
    calc_indextable
    calc_infoscript
    calc_innerstatementblock
    calc_lookupexpr
    calc_cmdlinecalc
    calc_methodoperator
    calc_mixedexpr
    calc_modellinkmethodexpr
    calc_modellinkinit
    calc_modellinkmethodstatement
    calc_nonspatialimpl
    calc_nonspatialtssimpl
    calc_objcount
    calc_operator
    calc_parameter
    calc_parsindex
    calc_parspar
    calc_posexception
    calc_report
    calc_spatialimpl
    calc_stackcr
    calc_stackinput
    calc_statement
    calc_statementblock
    calc_stdoutstat
    calc_subparameter
    calc_symbol
    calc_lookuptableleaf
    calc_lookuptableparameter
    calc_lookuptable
    calc_lookuplinear
    calc_timeinputexpr
    calc_timeoutput
    calc_tssinputparameter
    calc_timetable
    calc_tssinputleaf
    calc_tssoutputparameter
    calc_tssoutputvalue
    calc_tssparameter
    calc_usedefnode
    calc_usermodellink
    calc_usersymbol
    calc_vs
    calc_writeinfo
    calc_timervalue
    calc_symboltable
    calc_defpar
    calc_usepar
    calc_map2csf
    lexvaluefile
    lexnewcalc
    Parser
    calcparser
    calc_externalsymbols
    calc_findsymbol
    pcrcalc
    calc_arcviewextapi
    dassfunc
    dbinfunc
    dunfunc
    genfunc
    globfunc
    sbinfunc
    sunfunc
)

target_include_directories(pcraster_old_calc_objects
    PRIVATE
        $<TARGET_PROPERTY:pcraster::calc,INTERFACE_INCLUDE_DIRECTORIES>
        $<TARGET_PROPERTY:pcraster::pccts,INTERFACE_INCLUDE_DIRECTORIES>
        $<TARGET_PROPERTY:pcraster::pcrxml,INTERFACE_INCLUDE_DIRECTORIES>
        $<TARGET_PROPERTY:pcraster::tab,INTERFACE_INCLUDE_DIRECTORIES>
)

target_include_directories(pcraster_old_calc_objects
    PUBLIC
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
)

target_compile_definitions(pcraster_old_calc_objects
    PRIVATE
        USE_IN_PCRCALC
        YY_NO_UNISTD_H
        YY_NEVER_INTERACTIVE
)

add_library(pcraster_old_calc SHARED
    $<TARGET_OBJECTS:pcraster_old_calc_objects>
)

add_library(pcraster::old_calc ALIAS pcraster_old_calc)

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
        DESTINATION lib COMPONENT Runtime
    LIBRARY
        DESTINATION lib COMPONENT Runtime
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
            calc_tssoutputvaluetest
            calc_maskcompressortest
            calc_manualexampletestertest
            calc_manualexamplestest
            calc_nonspatialtest
            calc_spatialtest
            calc_lookuptabletest
            calc_lookuplineartest
            # calc_clientinterfacetest.cc  No longer supported.
            calc_rundirectorytest
            calc_iocsffieldstrategytest
            # calc_iobandfieldstrategytest.cc  No longer supported.
            # calc_iobandtest.cc  No longer supported.
            calc_parsertest
            calc_lexinputtest
            calc_modelbuildertest
            # calc_wldelfthabitattest.cc  No longer supported.
        OBJECT_LIBRARIES
            $<TARGET_OBJECTS:pcraster_old_calc_objects>
        LINK_LIBRARIES
            $<TARGET_PROPERTY:pcraster_old_calc,LINK_LIBRARIES>
    )
    add_test(
        NAME setup_oldcalc
        COMMAND ${PYTHON_EXECUTABLE} test_prolog.py
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
