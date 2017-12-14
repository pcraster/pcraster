add_library(tab STATIC
    cross
    looktab
    lookup
    readltab
    table
    writltab
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
            tab_classcountmaptest
            tab_classintervalmaptest
        LINK_LIBRARIES
            pcraster::tab
    )
endif()
