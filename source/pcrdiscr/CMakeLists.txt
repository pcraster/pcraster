include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}/../pcraster_dal
)

SET(SOURCES
    discr_block.cc
    discr_raster.cc
    discr_voxelstack.cc
)
SET(TEST_SOURCES
    discr_blocktest.cc
    discr_blockdatatest.cc
    discr_rasterdatatest.cc
    discr_rastertest.cc
    discr_test.cc
    discr_voxelstacktest.cc
)
ADD_LIBRARY(pcrdiscr
    ${SOURCES}
)
ADD_EXECUTABLE(test_pcrdiscr
    ${TEST_SOURCES}
)
TARGET_LINK_LIBRARIES(test_pcrdiscr
    pcrdiscr
)
ADD_CUSTOM_TARGET(run_test_pcrdiscr
    COMMAND test_pcrdiscr --detect_memory_leaks=0
    DEPENDS test_pcrdiscr
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
)
ADD_DEPENDENCIES(tests run_test_pcrdiscr)
