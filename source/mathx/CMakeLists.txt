add_library(mathx STATIC
    dirconv.c
    dirstat.c
    ran.c
    recipes.c
    rint.c
)

add_library(pcraster::mathx ALIAS mathx)

target_include_directories(mathx
    PRIVATE
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/../include>
)

target_include_directories(mathx
    PUBLIC
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
)

target_link_libraries(mathx
    pcraster::misc
)

if(PCRASTER_BUILD_TEST)
    add_unit_tests(
        SCOPE mathx
        NAMES
            dirconvtest.cc
        LINK_LIBRARIES
            pcraster::mathx
    )
endif()
