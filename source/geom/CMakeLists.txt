include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}/../misc
    ${CMAKE_CURRENT_SOURCE_DIR}/../mathx
)

SET(SOURCES
    point.c
    rect.c
)

set_source_files_properties(${SOURCES} PROPERTIES LANGUAGE CXX)

ADD_LIBRARY(geom
    ${SOURCES}
)
TARGET_LINK_LIBRARIES(geom
    mathx
)


if(PCRASTER_BUILD_TEST)
    set(TEST_NAMES
        geom_pointtest
    )
    add_unit_tests(
        SCOPE geom
        NAMES ${TEST_NAMES}
        LINK_LIBRARIES
            geom
    )
endif()
