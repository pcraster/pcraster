add_library(pcrblock STATIC
    block_add
    block_arithmeticoperators
    block_block
    block_cast
    block_claycompactor
    block_compare
    block_dehaancompactor
    block_dummycompactor
    block_io
    block_profile
    block_remove
    block_resample
    block_sandcompactor
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
        LINK_LIBRARIES
            pcraster::pcrblock
    )
endif()
