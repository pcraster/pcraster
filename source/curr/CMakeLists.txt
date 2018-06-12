add_library(curr STATIC
    currmenu
    keyfunc
)

add_library(pcraster::curr ALIAS curr)

target_include_directories(curr
    PUBLIC
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
        ${CURSES_INCLUDE_DIRS}
)

target_link_libraries(curr
    PUBLIC
        pcraster::misc
        ${CURSES_LIBRARIES}
)
