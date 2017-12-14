add_library(pcrvoxelstack STATIC
    voxelstack_voxelstack
)

add_library(pcraster::pcrvoxelstack ALIAS pcrvoxelstack)

target_include_directories(pcrvoxelstack
    PUBLIC
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
)

target_link_libraries(pcrvoxelstack
    PUBLIC
        pcraster::pcrdiscr
)

if(PCRASTER_BUILD_TEST)
    add_unit_tests(
        SCOPE voxelstack
        NAMES
            voxelstack_voxelstacktest
        LINK_LIBRARIES
            pcraster::pcrvoxelstack
    )
endif()
