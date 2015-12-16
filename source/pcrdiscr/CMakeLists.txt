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


###     ADD_EXECUTABLE(test_pcrdiscr
###         ${TEST_NAMES}
###     )
###     TARGET_LINK_LIBRARIES(test_pcrdiscr
###         pcrdiscr
###     )
###     ADD_CUSTOM_TARGET(run_test_pcrdiscr
###         COMMAND test_pcrdiscr --detect_memory_leaks=0
###         DEPENDS test_pcrdiscr
###         WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
###     )
###     ADD_DEPENDENCIES(tests run_test_pcrdiscr)
endif()
