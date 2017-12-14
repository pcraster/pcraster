add_library(misc STATIC
    aterror
    bitset2d
    bitset
    chkmem
    cnvrtnum
    error
    exstring
    fileio
    fileset
    filestat
    mallocxd
    qsortcmp
    recmem
    simplex
    swapbyte
)

add_library(pcraster::misc ALIAS misc)

target_include_directories(misc
    PUBLIC
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/../include>
        $<TARGET_PROPERTY:pcraster::raster_format,INTERFACE_INCLUDE_DIRECTORIES>
)
