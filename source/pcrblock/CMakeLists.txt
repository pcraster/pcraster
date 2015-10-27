include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}/../pcraster_dal
    ${CMAKE_CURRENT_SOURCE_DIR}/../pcrdiscr
)

SET(SOURCES
    block_add.cc
    block_arithmeticoperators.cc
    block_block.cc
    block_cast.cc
    block_claycompactor.cc
    block_compare.cc
    block_dehaancompactor.cc
    block_dummycompactor.cc
    block_io.cc
    block_profile.cc
    block_remove.cc
    block_resample.cc
    block_sandcompactor.cc
)
ADD_LIBRARY(pcrblock
    ${SOURCES}
)


if(FALSE AND PCRASTER_BUILD_TEST)
    SET(TEST_SOURCES
        block_addtest.cc
        block_arithmeticoperatorstest.cc
        block_blocktest.cc
        block_casttest.cc
        block_comparetest.cc
        block_compactorstest.cc
        block_dehaancompactortest.cc
        block_iotest.cc
        block_mackeybridgecompactortest.cc
        block_removetest.cc
        block_profiletest.cc
        block_resampletest.cc
        block_test.cc
        block_voxelatheighttest.cc
    )
    ADD_EXECUTABLE(test_pcrblock
        ${TEST_SOURCES}
    )
    TARGET_LINK_LIBRARIES(test_pcrblock
        pcrblock
        pcrdiscr
        pcraster_dal
    )
    ADD_CUSTOM_TARGET(run_test_pcrblock
        COMMAND test_pcrblock --detect_memory_leaks=0
        DEPENDS test_pcrblock
        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    )
    ADD_DEPENDENCIES(tests run_test_pcrblock)
endif()
