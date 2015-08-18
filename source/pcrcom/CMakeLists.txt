CONFIGURE_FILE(
    ${CMAKE_CURRENT_SOURCE_DIR}/testrun.prolog.in
    ${CMAKE_CURRENT_BINARY_DIR}/testrun.prolog
    @ONLY
)
CONFIGURE_FILE(
    ${CMAKE_CURRENT_SOURCE_DIR}/testrun.epilog.in
    ${CMAKE_CURRENT_BINARY_DIR}/testrun.epilog
    @ONLY
)


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
SET(TEST_SOURCES
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
    com_cpucyclecountertest.cc
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
    com_rleptrvectortest.cc
    com_singlevaluedrastertest.cc
    com_spawntest.cc
    # deprecated, see InTableStream docs
    # com_intablestreamtest.cc
    # com_tableinfotest.cc
    # com_spiritfilelineparsertest.cc
    com_spiritfileparsertest.cc
    com_statisticstest.cc
    com_strlibtest.cc
    com_tabletest.cc
    com_tempdirectorytest.cc
    com_test.cc
    com_unorderedcrosstabletest.cc
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
    SET(TEST_SOURCES ${TEST_SOURCES} com_win32registrykeytest.cc)
ENDIF(WIN32)
ADD_LIBRARY(pcrcom
    ${SOURCES}
)
ADD_EXECUTABLE(test_pcrcom
    ${TEST_SOURCES}
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

TARGET_LINK_LIBRARIES(test_pcrcom
    pcrcom
)

ADD_CUSTOM_TARGET(run_test_pcrcom
    COMMAND bash ./testrun.prolog
    COMMAND test_pcrcom --detect_memory_leaks=0
    COMMAND bash ./testrun.epilog
    DEPENDS test_pcrcom
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
)
ADD_DEPENDENCIES(tests run_test_pcrcom)
