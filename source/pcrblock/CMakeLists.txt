add_library(pcrblock STATIC
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

add_library(pcraster::pcrblock ALIAS pcrblock)

target_include_directories(pcrblock
    PUBLIC
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
)

target_link_libraries(pcrblock
    PUBLIC
        pcraster::dal
        pcraster::pcrdiscr
)

if(PCRASTER_BUILD_TEST)
    add_unit_tests(
        SCOPE pcrblock
        NAMES
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
            block_voxelatheighttest.cc
        LINK_LIBRARIES
            pcraster::pcrblock
    )
endif()
