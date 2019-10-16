add_library(curr STATIC
    currmenu
    keyfunc
)

add_library(pcraster::curr ALIAS curr)

target_include_directories(curr
    PUBLIC
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
        $<$<BOOL:${UNIX}>:${CURSES_INCLUDE_DIRS}>
)

# Required when using pdcurses as dll,
# otherwise showing up of menu will fail
# target_compile_definitions(curr
#     PUBLIC
#         $<$<BOOL:${WIN32}>: -DPDC_DLL_BUILD>
# )

target_link_libraries(curr
    PUBLIC
        pcraster::misc
        $<$<BOOL:${UNIX}>:${CURSES_LIBRARIES}>
        $<$<BOOL:${WIN32}>:pcraster::pdcurses>
)
