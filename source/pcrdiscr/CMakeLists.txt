add_library(pcrdiscr STATIC
    discr_block
    discr_raster
    discr_voxelstack
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
            discr_blocktest
            discr_blockdatatest
            discr_rasterdatatest
            discr_rastertest
            discr_voxelstacktest
        LINK_LIBRARIES
            pcrdiscr
    )
endif()
