add_library(pcrgeom STATIC
    geom_line
    geom_point
    geom_util
)

add_library(pcraster::pcrgeom ALIAS pcrgeom)

target_include_directories(pcrgeom
    PUBLIC
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
        $<TARGET_PROPERTY:pcraster::pcrgeo,INTERFACE_INCLUDE_DIRECTORIES>
)
