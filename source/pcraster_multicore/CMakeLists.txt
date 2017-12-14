### # OLS: check if this is safe to apply to the source directory instead
### include_directories(
###   ${CMAKE_CURRENT_SOURCE_DIR}/../
###   ${CMAKE_CURRENT_BINARY_DIR}/../pcraster_python # only required to pick up export header
### )

add_subdirectory(python)

if(PCRASTER_BUILD_TEST)
    add_subdirectory(tests)
endif()
