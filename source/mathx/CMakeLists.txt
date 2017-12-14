add_library(mathx STATIC
    dirconv
    dirstat
    ran
    recipes
    rint
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
            dirconvtest
        LINK_LIBRARIES
            pcraster::mathx
    )
endif()
