### # TODO move this to UseAdvApi32.cmake
### IF(WIN32)
###     FIND_LIBRARY(ADVAPI32_LIBRARY
###         NAMES
###         advapi32
###         PATHS
###         $ENV{VSINSTALLDIR}/VC/PlatformSDK/Lib
###     )
###     INCLUDE_DIRECTORIES(
###         # win32exceptiontext.inc
###         ${CMAKE_CURRENT_SOURCE_DIR}/../include
###     )
### ENDIF()

add_library(pcrcom
    com_appargs.cc
    # mam/puma only
    # com_app.cc
    # com_apphelper.cc
    # com_commandline.cc
    # com_commandlineapp.cc
    # com_commandlineargument.cc
    # com_commandlinearguments.cc
    # com_commandmodeargument.cc
    # com_exclusiveargument.cc
    com_basictable.cc
    com_cliberror.cc
    com_countedobject.cc
    # com_cpucyclecounter.cc
    com_csfcell.cc
    com_cumprobabilityclassifier.cc
    com_directory.cc
    com_dimap.cc
    com_rimap.cc
    com_dynamiclibrary.cc
    com_exception.cc
    com_file.cc
    com_filemap.cc
    com_icommandmode.cc
    com_ilabeledprogresstracker.cc
    com_interval.cc
    com_intervalmap.cc
    com_iprogressbar.cc
    com_iprogresscountdown.cc
    com_keyvaluetable.cc
    com_math.cc
    com_observer.cc
    com_pathinfo.cc
    com_pathname.cc
    com_progressbar.cc
    com_progresstracker.cc
    # com_raster.cc
    # com_repeatableargument.cc
    # com_repeatableexclusiveargument.cc
    # com_singlevaluedraster.cc
    com_spawn.cc
    # deprecated, see InTableStream docs
    # com_tableinfo.cc
    # com_spiritfilelineparser.cc
    # com_intablestream.cc
    com_spiritfileparser.cc
    # com_statistics.cc
    com_streamwriter.cc
    com_strlib.cc
    com_subject.cc
    com_table.cc
    com_tempdirectory.cc
    com_templates.cc
    com_toggle.cc
    com_tune.cc
    com_uniquestringgenerator.cc
    com_unorderedcrosstable.cc

    # Registry API WIN32 LIB.
    $<$<BOOL:${WIN32}>:com_win32.cc com_win32registrykey.cc>
)

add_library(pcraster::pcrcom ALIAS pcrcom)

target_include_directories(pcrcom
    PUBLIC
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
)

target_link_libraries(pcrcom
    PUBLIC
        pcraster::misc
        # For dlopen in com_dynamiclibrary.
        $<$<BOOL:${UNIX}>:${CMAKE_DL_LIBS}>
    PRIVATE
        Boost::headers
)

### IF(WIN32)
###     TARGET_LINK_LIBRARIES(pcrcom ${ADVAPI32_LIBRARY})
### ENDIF(WIN32)

if(PCRASTER_BUILD_TEST)

    configure_file(
        test_prolog.py.in
        test_prolog.py
        @ONLY
    )

    if(WIN32)
        set(additional_test_sources
            com_win32registrykeytest.cc)
    endif()

    add_unit_tests(
        SCOPE pcrcom
        NAMES
            com_algorithmtest.cc
            com_anypointerstest.cc
            com_binaryoperatorstest.cc
            com_clonetest.cc
            # mam only
            # com_commandlineargumentstest.cc
            # com_commandlineargumenttest.cc
            # com_commandlinetest.cc
            # com_argumentparsertest.cc
            com_appargstest.cc
            # com_cpucyclecountertest.cc
            com_csfcelltest.cc
            com_directorytest.cc
            com_filemaptest.cc
            com_filetest.cc
            com_functionstest.cc
            com_intervalmaptest.cc
            com_intervaltest.cc
            com_irastertest.cc
            com_keyvaluetabletest.cc
            com_labeledprogresstrackertest.cc
            com_mathtest.cc
            com_mvgenerictest.cc
            com_mvoptest.cc
            com_parserstest.cc
            com_pathinfotest.cc
            com_pathnametest.cc
            com_progressbartest.cc
            com_rastertest.cc
            com_singlevaluedrastertest.cc
            com_spawntest.cc
            # deprecated, see InTableStream docs
            # com_intablestreamtest.cc
            # com_tableinfotest.cc
            # com_spiritfilelineparsertest.cc
            com_spiritfileparsertest.cc
            # com_statisticstest.cc
            com_strlibtest.cc
            com_tabletest.cc
            com_tempdirectorytest.cc
            com_unorderedcrosstabletest.cc

            # Generator expressions don't work here
            # $<$<BOOL:${WIN32}>:com_win32registrykeytest.cc>
            ${additional_test_sources}
        LINK_LIBRARIES
            pcraster::pcrcom
    )

    add_test(
        NAME setup_pcrcom
        COMMAND ${Python_EXECUTABLE} test_prolog.py
    )

    set_tests_properties(setup_pcrcom
        PROPERTIES
            FIXTURES_SETUP
                FixturePcrCom
    )

    set_tests_properties(
            pcrcom_com_algorithmtest
            pcrcom_com_filetest
            pcrcom_com_directorytest
        PROPERTIES
            FIXTURES_REQUIRED
                FixturePcrCom
    )
endif()
