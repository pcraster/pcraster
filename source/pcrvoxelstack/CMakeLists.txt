include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}/../pcrdiscr
    ${CMAKE_CURRENT_SOURCE_DIR}/../pcrfunc
)

SET(SOURCES
    voxelstack_voxelstack.cc
)
ADD_LIBRARY(pcrvoxelstack
    ${SOURCES}
)


if(PCRASTER_BUILD_TEST)
    set(TEST_NAMES
        voxelstack_voxelstacktest
    )
    add_unit_tests(
        SCOPE voxelstack
        NAMES ${TEST_NAMES}
    )
endif()
