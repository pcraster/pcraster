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
        ### # ${LIBRARIES_TO_LINK}
        app
        pcrcom
        tab
        mathx
        misc
        pcraster_dal
        ### # ${PCRASTER_RASTER_FORMAT_LIBRARIES}
        ### # ${DEVENV_UTILS_LIBRARY}
        ### # ${QT_QTCORE_LIBRARIES}
    )
# ENDIF(WIN32)

SET_TARGET_PROPERTIES(col2map PROPERTIES LINKER_LANGUAGE CXX)

INSTALL(
    TARGETS col2map
    RUNTIME DESTINATION bin COMPONENT Runtime
)


if(PCRASTER_BUILD_TEST)
    set(TEST_NAMES
        col2map_col2maptest
    )

    add_unit_tests(
        SCOPE col2map
        NAMES ${TEST_NAMES}
        LINK_LIBRARIES
            pcraster_dal
        DEPENDENCIES
            mapattr col2map
    )
endif()
