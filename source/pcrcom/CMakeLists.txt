
# TODO move this to UseAdvApi32.cmake
IF(WIN32)
    FIND_LIBRARY(ADVAPI32_LIBRARY
        NAMES
        advapi32
        PATHS
        $ENV{VSINSTALLDIR}/VC/PlatformSDK/Lib
    )
    INCLUDE_DIRECTORIES(
        # win32exceptiontext.inc
        ${CMAKE_CURRENT_SOURCE_DIR}/../include
    )
ENDIF()

INCLUDE_DIRECTORIES(
    ${CMAKE_CURRENT_SOURCE_DIR}/../misc
)
# Moduls with only template definitions, no public symbols.
# com_exclusiveargument.cc
# com_labeledprogresstracked.cc
# com_labeledprogresstracker.cc
# com_legend.cc
# com_legendclass.cc
# com_raster.cc
# com_repeatableargument.cc
# com_repeatableexclusiveargument.cc
# com_rleptrvector.cc
# com_singlevaluedraster.cc
# com_statistics.cc
# com_progresstracked.cc
SET(SOURCES
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
    com_cpucyclecounter.cc
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
    # com_rleptrvector.cc
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
)








# # LSB does not define mallopt
# IF(CMAKE_COMPILER_IS_GNUCC)
#     IF(${LSB_BUILD})
#         SET_SOURCE_FILES_PROPERTIES(
#             com_tune.cc
#             PROPERTIES COMPILE_FLAGS -DPCRASTER_LSB
#         )
#         SET_SOURCE_FILES_PROPERTIES(
#             com_mvoptest.cc
#             PROPERTIES COMPILE_FLAGS -DPCRASTER_LSB
#         )
#     ENDIF(${LSB_BUILD})
# ENDIF(CMAKE_COMPILER_IS_GNUCC)

IF(WIN32)
    # Registry API WIN32 LIB.
    SET(SOURCES ${SOURCES} com_win32.cc com_win32registrykey.cc)
ENDIF(WIN32)
ADD_LIBRARY(pcrcom
    ${SOURCES}
)
TARGET_LINK_LIBRARIES(pcrcom
    ${Boost_FILESYSTEM_LIBRARY}
    ${Boost_SYSTEM_LIBRARY}
)
IF(WIN32)
    TARGET_LINK_LIBRARIES(pcrcom ${ADVAPI32_LIBRARY})
ENDIF(WIN32)
IF(UNIX)
    # For dlopen in com_dynamiclibrary.
    TARGET_LINK_LIBRARIES(pcrcom dl)
ENDIF(UNIX)


if(PCRASTER_BUILD_TEST)

    configure_file(
        ${CMAKE_CURRENT_SOURCE_DIR}/test_prolog.py.in
        ${CMAKE_CURRENT_BINARY_DIR}/test_prolog.py
        @ONLY
    )

    set(TEST_NAMES
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
        com_rleptrvectortest
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
    )
    IF(WIN32)
        SET(TEST_NAMES ${TEST_NAMES}
            com_win32registrykeytest)
    ENDIF(WIN32)
    add_unit_tests(
        SCOPE pcrcom
        NAMES ${TEST_NAMES}
        LINK_LIBRARIES
            pcrcom
    )

    add_test(
        NAME setup_pcrcom
        COMMAND ${PYTHON_EXECUTABLE} test_prolog.py
    )

    set_tests_properties(setup_pcrcom
        PROPERTIES FIXTURES_SETUP FixturePcrCom
    )

    set_tests_properties(pcrcom_com_algorithmtest PROPERTIES
        FIXTURES_REQUIRED
          FixturePcrCom
    )

endif()
