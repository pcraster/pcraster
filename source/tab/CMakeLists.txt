add_library(tab STATIC
    cross.c
    looktab.c
    lookup.c
    readltab.c
    table.c
    writltab.c
)

add_library(pcraster::tab ALIAS tab)

target_include_directories(tab
    PUBLIC
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
)

target_link_libraries(tab
    PRIVATE
        pcraster::app
    PUBLIC
        pcraster::pcrcom
)

if(PCRASTER_BUILD_TEST)
    add_unit_tests(
        SCOPE tab
        NAMES
            tab_classcountmaptest.cc
            tab_classintervalmaptest.cc
        LINK_LIBRARIES
            pcraster::tab
    )
endif()
