include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}/../pcraster_dal
    ${CMAKE_CURRENT_SOURCE_DIR}/../pcrdiscr
    ${CMAKE_CURRENT_SOURCE_DIR}/../pcrfunc
)

SET(SOURCES
    raster_io.cc
    raster_raster.cc
)
ADD_LIBRARY(pcrraster
    ${SOURCES}
)


if(FALSE AND PCRASTER_BUILD_TEST)
    SET(TEST_SOURCES
        raster_iotest.cc
        raster_rastertest.cc
        raster_test.cc
    )
    ADD_EXECUTABLE(test_pcrraster
        ${TEST_SOURCES}
    )
    ADD_CUSTOM_TARGET(run_test_pcrraster
        COMMAND test_pcrraster
        DEPENDS test_pcrraster
        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    )
    ADD_DEPENDENCIES(tests run_test_pcrraster)
endif()
