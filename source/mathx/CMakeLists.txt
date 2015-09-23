include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}/../misc
)

SET(SOURCES
    dirconv.c
    dirstat.c
    ran.c
    recipes.c
    rint.c
)
ADD_LIBRARY(mathx
    ${SOURCES}
)


if(PCRASTER_BUILD_TEST)
    set(TEST_NAMES
        dirconvtest
    )
    add_unit_tests(
        SCOPE mathx
        NAMES ${TEST_NAMES}
        LINK_LIBRARIES
            mathx
    )
endif()
