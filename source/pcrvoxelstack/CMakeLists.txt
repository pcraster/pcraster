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


if(FALSE AND PCRASTER_BUILD_TEST)
    SET(TEST_SOURCES
        voxelstack_voxelstacktest.cc
        voxelstack_test.cc
    )
    ADD_EXECUTABLE(test_pcrvoxelstack
        ${TEST_SOURCES}
    )
    TARGET_LINK_LIBRARIES(test_pcrvoxelstack
        pcrvoxelstack
    )
    ADD_CUSTOM_TARGET(run_test_pcrvoxelstack
        COMMAND test_pcrvoxelstack
        DEPENDS test_pcrvoxelstack
        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    )
    ADD_DEPENDENCIES(tests run_test_pcrvoxelstack)
endif()
