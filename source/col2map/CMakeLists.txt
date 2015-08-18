CONFIGURE_FILE(
    ${CMAKE_CURRENT_SOURCE_DIR}/testrun.prolog.in
    ${CMAKE_CURRENT_BINARY_DIR}/testrun.prolog
    @ONLY
    NEWLINE_STYLE UNIX
)

include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}/../include
    ${CMAKE_CURRENT_SOURCE_DIR}/../app
    ${CMAKE_CURRENT_SOURCE_DIR}/../mathx
    ${CMAKE_CURRENT_SOURCE_DIR}/../misc
    ${CMAKE_CURRENT_SOURCE_DIR}/../pcraster_dal
    ${CMAKE_CURRENT_SOURCE_DIR}/../tab
    ${CMAKE_CURRENT_SOURCE_DIR}/../utils
)

# INCLUDE_DIRECTORIES(
#     ${PROJECT_SOURCE_DIR}/sources/include
#     ../mathx
#     ../misc
#     ../app
#     ../tab
# )

# SET(LIBRARIES_TO_LINK
#     app
#     pcrcom
#     tab
#     mathx
#     misc
#     ${PCRASTER_DAL_LIBRARY}
#     ${PCRASTER_RASTERFORMAT_LIBRARY}
#     ${DEVENV_UTILS_LIBRARY}
#     ${QT_QTCORE_LIBRARIES}
# )

SET(SOURCES
    col2map.c
    main.c
)

ADD_EXECUTABLE(col2map
    ${SOURCES}
)


# IF(WIN32)
#     TARGET_LINK_LIBRARIES(col2map
#         ${LIBRARIES_TO_LINK}
#         ${BOOST_FILESYSTEM_LIBRARY}
#         ${BOOST_SYSTEM_LIBRARY}
#     )
# ELSE(WIN32)
    TARGET_LINK_LIBRARIES(col2map
        # ${LIBRARIES_TO_LINK}
        app
        pcrcom
        tab
        mathx
        misc
        pcraster_dal
        # ${PCRASTER_RASTER_FORMAT_LIBRARIES}
        # ${DEVENV_UTILS_LIBRARY}
        # ${QT_QTCORE_LIBRARIES}
    )
# ENDIF(WIN32)

SET_TARGET_PROPERTIES(col2map PROPERTIES LINKER_LANGUAGE CXX)


SET(TEST_SOURCES
    col2map_test.cc
    col2map_col2maptest.cc
)

# this test just test some general properties of the underlying
# things
ADD_EXECUTABLE(test_col2map
    ${TEST_SOURCES}
)

ADD_DEPENDENCIES(test_col2map mapattr col2map)

TARGET_LINK_LIBRARIES(test_col2map
    pcraster_dal
    # ${LIBRARIES_TO_LINK}
    # ${BOOST_FILESYSTEM_LIBRARY}
    ${Boost_SYSTEM_LIBRARY}
)

ADD_CUSTOM_TARGET(run_test_col2map
    COMMAND bash ./testrun.prolog
    COMMAND test_col2map
    DEPENDS test_col2map
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
)
ADD_DEPENDENCIES(tests run_test_col2map)

INSTALL(
    TARGETS col2map
    RUNTIME DESTINATION bin COMPONENT Runtime
)
