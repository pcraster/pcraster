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

### IF(WIN32)
###     # Registry API WIN32 LIB.
###     SET(SOURCES ${SOURCES} com_win32.cc com_win32registrykey.cc)
### ENDIF(WIN32)

add_library(pcrcom
    com_appargs
    # mam/puma only
    # com_app
    # com_apphelper
    # com_commandline
    # com_commandlineapp
    # com_commandlineargument
    # com_commandlinearguments
    # com_commandmodeargument
    # com_exclusiveargument
    com_basictable
    com_cliberror
    com_countedobject
    com_cpucyclecounter
    com_csfcell
    com_cumprobabilityclassifier
    com_directory
    com_dimap
    com_rimap
    com_dynamiclibrary
    com_exception
    com_file
    com_filemap
    com_icommandmode
    com_ilabeledprogresstracker
    com_interval
    com_intervalmap
    com_iprogressbar
    com_iprogresscountdown
    com_keyvaluetable
    com_math
    com_observer
    com_pathinfo
    com_pathname
    com_progressbar
    com_progresstracker
    # com_raster
    # com_repeatableargument
    # com_repeatableexclusiveargument
    # com_singlevaluedraster
    com_spawn
    # deprecated, see InTableStream docs
    # com_tableinfo
    # com_spiritfilelineparser
    # com_intablestream
    com_spiritfileparser
    # com_statistics
    com_streamwriter
    com_strlib
    com_subject
    com_table
    com_tempdirectory
    com_templates
    com_toggle
    com_tune
    com_uniquestringgenerator
    com_unorderedcrosstable

    # Registry API WIN32 LIB.
    $<$<BOOL:${WIN32}>:com_win32 com_win32registrykey>
)

add_library(pcraster::pcrcom ALIAS pcrcom)

target_include_directories(pcrcom
    PUBLIC
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
)

target_link_libraries(pcrcom
    PUBLIC
        pcraster::misc
        Boost::filesystem

        # For dlopen in com_dynamiclibrary.
        $<$<BOOL:${UNIX}>:dl>
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
            com_win32registrykeytest)
    endif()

    add_unit_tests(
        SCOPE pcrcom
        NAMES
            com_algorithmtest
            com_anypointerstest
            com_binaryoperatorstest
            com_clonetest
            # mam only
            # com_commandlineargumentstest
            # com_commandlineargumenttest
            # com_commandlinetest
            # com_argumentparsertest
            com_appargstest
            com_cpucyclecountertest
            com_csfcelltest
            com_directorytest
            com_filemaptest
            com_filetest
            com_functionstest
            com_intervalmaptest
            com_intervaltest
            com_irastertest
            com_keyvaluetabletest
            com_labeledprogresstrackertest
            com_mathtest
            com_mvgenerictest
            com_mvoptest
            com_parserstest
            com_pathinfotest
            com_pathnametest
            com_progressbartest
            com_rastertest
            com_singlevaluedrastertest
            com_spawntest
            # deprecated, see InTableStream docs
            # com_intablestreamtest
            # com_tableinfotest
            # com_spiritfilelineparsertest
            com_spiritfileparsertest
            com_statisticstest
            com_strlibtest
            com_tabletest
            com_tempdirectorytest
            com_unorderedcrosstabletest

            # Generator expressions don't work here
            # $<$<BOOL:${WIN32}>:com_win32registrykeytest>
            ${additional_test_sources}
        LINK_LIBRARIES
            pcraster::pcrcom
    )

    add_test(
        NAME setup_pcrcom
        COMMAND ${PYTHON_EXECUTABLE} test_prolog.py
    )

    set_tests_properties(setup_pcrcom
        PROPERTIES
            FIXTURES_SETUP
                FixturePcrCom
    )

    set_tests_properties(pcrcom_com_algorithmtest pcrcom_com_filetest
        PROPERTIES
            FIXTURES_REQUIRED
                FixturePcrCom
    )
endif()
