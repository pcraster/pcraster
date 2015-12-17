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


if(PCRASTER_BUILD_TEST)
    set(TEST_NAMES
        raster_iotest
        raster_rastertest.cc
    )
    add_unit_tests(
        SCOPE pcrraster
        NAMES ${TEST_NAMES}
    )
endif()
