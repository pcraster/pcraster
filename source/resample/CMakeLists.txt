CONFIGURE_FILE(
    ${CMAKE_CURRENT_SOURCE_DIR}/testrun.prolog.in
    ${CMAKE_CURRENT_BINARY_DIR}/testrun.prolog
    @ONLY
    NEWLINE_STYLE UNIX
)

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


# Unit testing stuff ----------------------------------------------------------
# run resample from the command line and compare output maps,
# done in testrun.prolog
ADD_CUSTOM_TARGET(run_test_resample
    COMMAND sed -i.tmp "s/CMAKE_CFG_INTDIR/${CMAKE_CFG_INTDIR}/g"
        ./testrun.prolog
    COMMAND bash ./testrun.prolog
    DEPENDS resample mapdiff
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
)
ADD_DEPENDENCIES(tests run_test_resample)


# Install ---------------------------------------------------------------------
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
        api
        app
        pcrcom
        geom
        tab
        mathx
        misc
        ${PCRASTER_RASTER_FORMAT_LIBRARIES}
    )
# ENDIF(WIN32)

INSTALL(
    TARGETS resample
    RUNTIME DESTINATION bin COMPONENT Runtime
)
