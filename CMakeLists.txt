cmake_minimum_required(VERSION 3.1)

project(pcraster)

execute_process(
    COMMAND date "+%Y"
    OUTPUT_VARIABLE DEV_CURRENT_YEAR
    OUTPUT_STRIP_TRAILING_WHITESPACE
)

set(${PROJECT_NAME}_ORGANISATION_NAME "PCRaster R&D Team")
set(${PROJECT_NAME}_COPYRIGHT "1990-${DEV_CURRENT_YEAR} PCRaster R&D Team")
set(${PROJECT_NAME}_PROJECT_NAME "${PROJECT_NAME}")
set(${PROJECT_NAME}_HOMEPAGE "http://www.pcraster.eu")
set(${PROJECT_NAME}_MAJOR_VERSION "5")
set(${PROJECT_NAME}_MINOR_VERSION "0")
set(${PROJECT_NAME}_PATCH_VERSION "0")
set(${PROJECT_NAME}_PRE_RELEASE_VERSION "-rc.1")
set(${PROJECT_NAME}_VERSION
    "${${PROJECT_NAME}_MAJOR_VERSION}.${${PROJECT_NAME}_MINOR_VERSION}.${${PROJECT_NAME}_PATCH_VERSION}${${PROJECT_NAME}_PRE_RELEASE_VERSION}")

set(CMAKE_MODULE_PATH
    $ENV{CMAKE_MODULE_PATH}
    ${CMAKE_CURRENT_SOURCE_DIR}/devbase/cmake
    ${CMAKE_CURRENT_SOURCE_DIR}/environment/cmake
)
include(PCRaster)


configure_file(
    ${CMAKE_CURRENT_SOURCE_DIR}/CTestCustom.cmake.in
    ${CMAKE_CURRENT_BINARY_DIR}/CTestCustom.cmake
)
configure_file(
    ${CMAKE_CURRENT_SOURCE_DIR}/test_run_prolog.sh.in
    ${CMAKE_CURRENT_BINARY_DIR}/test_run_prolog.sh
)
configure_file(
    ${CMAKE_CURRENT_SOURCE_DIR}/test_run_epilog.sh.in
    ${CMAKE_CURRENT_BINARY_DIR}/test_run_epilog.sh
)


if(CMAKE_CXX_COMPILER_ID STREQUAL "GNU" OR
        CMAKE_CXX_COMPILER_ID STREQUAL "Clang")

    set(CMAKE_C_FLAGS
        "${CMAKE_C_FLAGS} -Wno-cpp -Wno-parentheses -Wno-cast-qual -Wno-maybe-uninitialized -Wno-unused-parameter -Wno-empty-body -Wno-unused-but-set-variable -Wno-unused-function -Wno-format -Wno-switch -Wno-empty-body -Wno-sign-compare"
    )
    set(CMAKE_CXX_FLAGS
        "${CMAKE_CXX_FLAGS} -Wno-cpp -Wno-parentheses -Wno-cast-qual -Wno-maybe-uninitialized -Wno-unused-parameter -Wno-empty-body -Wno-unused-but-set-variable -Wno-deprecated-declarations -Wno-ignored-qualifiers -Wno-switch -Wno-pedantic -Wno-write-strings -Wno-unused-variable -Wno-unused-local-typedefs -Wno-delete-non-virtual-dtor -Wno-unused-label -Wno-sign-compare"
    )


endif()


add_subdirectory(source)
add_subdirectory(documentation)
