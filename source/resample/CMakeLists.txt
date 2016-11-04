include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}/../app
    ${CMAKE_CURRENT_SOURCE_DIR}/../geom
    ${CMAKE_CURRENT_SOURCE_DIR}/../mathx
    ${CMAKE_CURRENT_SOURCE_DIR}/../misc
    ${CMAKE_CURRENT_SOURCE_DIR}/../tab
)

SET(SOURCES
    main.c
    sample.c
    samplecl.c
    sampleco.c
    point.c
)

set_source_files_properties(${SOURCES} PROPERTIES LANGUAGE CXX)

ADD_EXECUTABLE(resample
    ${SOURCES}
)


# IF(WIN32)
#     TARGET_LINK_LIBRARIES(resample
#         api
#         app
#         pcrcom
#         geom
#         tab
#         mathx
#         misc
#         ${PCRASTER_RASTERFORMAT_LIBRARY}
#         ${BOOST_FILESYSTEM_LIBRARY}
#         ${BOOST_SYSTEM_LIBRARY}
#     )
# ELSE(WIN32)
    TARGET_LINK_LIBRARIES(resample
        # api
        app
        # pcrcom
        geom
        tab
        mathx
        # misc
        ${PCRASTER_RASTER_FORMAT_LIBRARIES}
    )
# ENDIF(WIN32)

INSTALL(
    TARGETS resample
    RUNTIME DESTINATION bin COMPONENT Runtime
)


if(FALSE OR PCRASTER_BUILD_TEST)
    # TODO Turn this test into a real unit test with system calls or so.

    # Running resample test cases...
    # ./testrun.prolog: line 16: .../pcraster/bin/CMAKE_CFG_INTDIR/resample: No such file or directory
    # Problem running command: ./test_run_prolog.sh
    # Problem executing pre-test command(s).
    # Errors while running CTest

    configure_file(
        ${CMAKE_CURRENT_SOURCE_DIR}/testrun.prolog.in
        ${CMAKE_CURRENT_BINARY_DIR}/testrun.prolog
        @ONLY
        NEWLINE_STYLE UNIX
    )

    # run resample from the command line and compare output maps,
    # done in testrun.prolog
    add_custom_target(run_test_resample
        COMMAND sed -i.tmp "s/CMAKE_CFG_INTDIR/${CMAKE_CFG_INTDIR}/g"
            ./testrun.prolog
        COMMAND bash ./testrun.prolog
        DEPENDS resample mapdiff
        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    )
    add_dependencies(tests run_test_resample)
endif()
