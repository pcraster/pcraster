add_executable(col2map
    col2map
    main
)

target_link_libraries(col2map
    PRIVATE
        pcraster::app
        pcraster::raster_format
        pcraster::tab
)

INSTALL(
    TARGETS
        col2map
    RUNTIME
        DESTINATION bin COMPONENT Runtime
)

install(
    TARGETS col2map
    RUNTIME
        DESTINATION bin COMPONENT Runtime
)


if(PCRASTER_BUILD_TEST)
    configure_file(
        test_prolog.py.in
        test_prolog.py
        @ONLY
        NEWLINE_STYLE UNIX
    )

    add_unit_tests(
        SCOPE col2map
        NAMES
            col2map_col2maptest
        LINK_LIBRARIES
            pcraster::dal
        DEPENDENCIES
            mapattr col2map
    )

    add_test(
        NAME setup_col2map_1
        COMMAND ${PYTHON_EXECUTABLE} test_prolog.py
    )

    set_tests_properties(setup_col2map_1
        PROPERTIES
            FIXTURES_SETUP FixtureCol2map_1
    )

#if [[ $OSTYPE != "cygwin" ]]
# MSC does not read Nan
# test not executed on windows

    add_test(
        NAME setup_col2map_2
        COMMAND mapattr -s -R 1 -C 3 -B nanclone.map
    )

    set_tests_properties(setup_col2map_2
        PROPERTIES
            FIXTURES_SETUP FixtureCol2map_2
    )

    set_tests_properties(setup_col2map_2
        PROPERTIES
            FIXTURES_REQUIRED FixtureCol2map_1
    )

    add_test(
        NAME setup_col2map_3
        COMMAND col2map -S --clone nanclone.map nan.col nan.map
    )

    set_tests_properties(setup_col2map_3
        PROPERTIES
            FIXTURES_SETUP FixtureCol2map_3
    )

    set_tests_properties(setup_col2map_3
        PROPERTIES
            FIXTURES_REQUIRED FixtureCol2map_2
    )

    set_tests_properties(col2map_col2map_col2maptest
        PROPERTIES
            FIXTURES_REQUIRED FixtureCol2map_3
    )
endif()
