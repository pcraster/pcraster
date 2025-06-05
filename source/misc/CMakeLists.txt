add_library(misc STATIC
    # aterror.c
    bitset2d.c
    bitset.c
    chkmem.c
    cnvrtnum.c
    error.c
    exstring.c
    # fileio.c
    # fileset.c
    filestat.c
    mallocxd.c
    qsortcmp.c
    # recmem.c
    simplex.c
    # swapbyte.c
)

add_library(pcraster::misc ALIAS misc)

target_include_directories(misc
    PUBLIC
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/../include>
        $<TARGET_PROPERTY:pcraster::raster_format,INTERFACE_INCLUDE_DIRECTORIES>
)
