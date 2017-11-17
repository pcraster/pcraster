include_directories(
    # ${CMAKE_CURRENT_SOURCE_DIR}/../include
    ${CMAKE_CURRENT_SOURCE_DIR}/../app
    ${CMAKE_CURRENT_SOURCE_DIR}/../mathx
    ${CMAKE_CURRENT_SOURCE_DIR}/../misc
    ${CMAKE_CURRENT_SOURCE_DIR}/../pcraster_dal
    ${CMAKE_CURRENT_SOURCE_DIR}/../tab
    # ${CMAKE_CURRENT_SOURCE_DIR}/../utils
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
        # pcrcom
        tab
        mathx
        # misc
        # pcraster_dal
        ### # pcraster_raster_format
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

    configure_file(
        ${CMAKE_CURRENT_SOURCE_DIR}/test_prolog.py.in
        ${CMAKE_CURRENT_BINARY_DIR}/test_prolog.py
        @ONLY
        NEWLINE_STYLE UNIX
    )

    add_test(NAME setup_col2map_1 COMMAND ${PYTHON_EXECUTABLE} test_prolog.py)
    set_tests_properties(setup_col2map_1
        PROPERTIES FIXTURES_SETUP FixtureCol2map_1
    )
#if [[ $OSTYPE != "cygwin" ]]
# MSC does not read Nan
# test not executed on windows
    add_test(NAME setup_col2map_2 COMMAND mapattr -s -R 1 -C 3 -B nanclone.map)
    set_tests_properties(setup_col2map_2
        PROPERTIES FIXTURES_SETUP FixtureCol2map_2
    )
    set_tests_properties(setup_col2map_2
        PROPERTIES FIXTURES_REQUIRED FixtureCol2map_1
    )

    add_test(NAME setup_col2map_3 COMMAND col2map -S --clone nanclone.map nan.col nan.map)
    set_tests_properties(setup_col2map_3
        PROPERTIES FIXTURES_SETUP FixtureCol2map_3
    )
    set_tests_properties(setup_col2map_3
        PROPERTIES FIXTURES_REQUIRED FixtureCol2map_2
    )

    set_tests_properties(col2map_col2map_col2maptest
        PROPERTIES FIXTURES_REQUIRED FixtureCol2map_3
    )

endif()
