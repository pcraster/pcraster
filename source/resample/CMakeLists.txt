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


if(PCRASTER_BUILD_TEST)

    configure_file(
        ${CMAKE_CURRENT_SOURCE_DIR}/test_prolog.py.in
        ${CMAKE_CURRENT_BINARY_DIR}/test_prolog.py
        @ONLY
        NEWLINE_STYLE UNIX
    )

    add_test(NAME setup_resample COMMAND ${PYTHON_EXECUTABLE} test_prolog.py)

    # sf463
    add_test(NAME test_resample_1 COMMAND resample --clone sf463_mask.map sf463_outlet.map sf463_output.map)
    add_test(NAME test_resample_2 COMMAND mapdiff validated/sf463_outlet_mask.map sf463_output.map)
    # sf485
    add_test(NAME test_resample_3 COMMAND resample -C sf485_dem.map sf485_output.map)
    add_test(NAME test_resample_4 COMMAND mapdiff validated/sf485_dem_cropped.map sf485_output.map)


    set_tests_properties(setup_resample
        PROPERTIES FIXTURES_SETUP FixtureResample
    )
    set_tests_properties(test_resample_1 test_resample_2 test_resample_3 test_resample_4
        PROPERTIES FIXTURES_REQUIRED FixtureResample
    )

endif()
