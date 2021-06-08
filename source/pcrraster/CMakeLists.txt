add_library(pcrraster STATIC
    raster_io.cc
    raster_raster.cc
)

add_library(pcraster::pcrraster ALIAS pcrraster)

target_include_directories(pcrraster
    PUBLIC
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
        $<TARGET_PROPERTY:pcraster::pcrfunc,INTERFACE_INCLUDE_DIRECTORIES>
)

target_link_libraries(pcrraster
    PUBLIC
        pcraster::dal
        pcraster::pcrdiscr
)

if(PCRASTER_BUILD_TEST)
    add_unit_tests(
        SCOPE pcrraster
        NAMES
            raster_iotest.cc
            raster_rastertest.cc
    )
endif()
