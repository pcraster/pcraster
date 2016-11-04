include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}/../pcraster_dal
)

SET(SOURCES
    discr_block.cc
    discr_raster.cc
    discr_voxelstack.cc
)
ADD_LIBRARY(pcrdiscr
    ${SOURCES}
)
target_link_libraries(pcrdiscr
    pcraster_dal
)

if(PCRASTER_BUILD_TEST)
    set(TEST_NAMES
        discr_blocktest
        discr_blockdatatest
        discr_rasterdatatest
        discr_rastertest
        discr_voxelstacktest
    )
    add_unit_tests(
        SCOPE pcrdiscr
        NAMES ${TEST_NAMES}
        LINK_LIBRARIES
            pcrdiscr
    )
endif()
