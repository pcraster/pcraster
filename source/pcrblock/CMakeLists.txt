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


if(PCRASTER_BUILD_TEST)
    set(TEST_NAMES
        block_addtest
        block_arithmeticoperatorstest
        block_blocktest
        block_casttest
        block_comparetest
        block_compactorstest
        block_dehaancompactortest
        block_iotest
        block_mackeybridgecompactortest
        block_removetest
        block_profiletest
        block_resampletest
        block_voxelatheighttest
    )
    add_unit_tests(
        SCOPE pcrblock
        NAMES ${TEST_NAMES}
        LINK_LIBRARIES
            pcrblock
            pcrdiscr
            pcraster_dal
    )
endif()
