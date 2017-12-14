add_executable(resample
    main
    sample
    samplecl
    sampleco
    point
)

target_link_libraries(resample
    PRIVATE
        pcraster::app
        pcraster::geom
        pcraster::raster_format
        pcraster::tab
)

install(
    TARGETS resample
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

    add_dependencies(resample
        mapdiff
    )

    add_test(
        NAME setup_resample
        COMMAND ${PYTHON_EXECUTABLE} test_prolog.py
    )

    # sf463
    add_test(
        NAME test_resample_1
        COMMAND resample --clone sf463_mask.map sf463_outlet.map sf463_output.map
    )

    add_test(
        NAME test_resample_2
        COMMAND mapdiff validated/sf463_outlet_mask.map sf463_output.map
    )

    # sf485
    add_test(
        NAME test_resample_3
        COMMAND resample -C sf485_dem.map sf485_output.map
    )

    add_test(
        NAME test_resample_4
        COMMAND mapdiff validated/sf485_dem_cropped.map sf485_output.map
    )

    set_tests_properties(setup_resample
        PROPERTIES
            FIXTURES_SETUP
                FixtureResample
    )

    set_tests_properties(test_resample_1 test_resample_3
        PROPERTIES
            FIXTURES_REQUIRED
                FixtureResample
    )
endif()
