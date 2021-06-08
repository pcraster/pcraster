add_library(pcrdiscr STATIC
    discr_block.cc
    discr_raster.cc
    discr_voxelstack.cc
)

add_library(pcraster::pcrdiscr ALIAS pcrdiscr)

target_include_directories(pcrdiscr
    PUBLIC
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
)

target_link_libraries(pcrdiscr
    pcraster::dal
)

if(PCRASTER_BUILD_TEST)
    add_unit_tests(
        SCOPE pcrdiscr
        NAMES
            discr_blocktest.cc
            discr_blockdatatest.cc
            discr_rasterdatatest.cc
            discr_rastertest.cc
            discr_voxelstacktest.cc
        LINK_LIBRARIES
            pcrdiscr
    )
endif()
