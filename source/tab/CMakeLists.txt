include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}/../app
    ${CMAKE_CURRENT_SOURCE_DIR}/../misc
    ${CMAKE_CURRENT_SOURCE_DIR}/../pcrcom
)

SET(SOURCES
    cross.c
    looktab.c
    lookup.c
    readltab.c
    table.c
    writltab.c
)
ADD_LIBRARY(tab
    ${SOURCES}
)
target_link_libraries(tab
    pcrcom
)

if(PCRASTER_BUILD_TEST)
    set(TEST_NAMES
        tab_classcountmaptest
        tab_classintervalmaptest
    )
    add_unit_tests(
        SCOPE tab
        NAMES ${TEST_NAMES}
        LINK_LIBRARIES
            pcrcom
    )
endif()
