add_library(geom STATIC
    point.cc
    rect.cc
)

add_library(pcraster::geom ALIAS geom)

target_include_directories(geom
    PUBLIC
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
)

target_include_directories(geom SYSTEM
    PRIVATE
        ${Boost_INCLUDE_DIRS}
)

target_link_libraries(geom
    PUBLIC
        pcraster::mathx
)

if(PCRASTER_BUILD_TEST)
    add_unit_tests(
        SCOPE geom
        NAMES
            geom_pointtest.cc
        LINK_LIBRARIES
            pcraster::geom
    )
endif()
