if(PCRASTER_BUILD_TEST)
    cmake_minimum_required(VERSION 3.7)
else()
    cmake_minimum_required(VERSION 3.1)
endif()

project(pcraster)

execute_process(
    COMMAND date "+%Y"
    OUTPUT_VARIABLE DEV_CURRENT_YEAR
    OUTPUT_STRIP_TRAILING_WHITESPACE
)

set(${PROJECT_NAME}_ORGANISATION_NAME "PCRaster Research and Development Team")
set(${PROJECT_NAME}_COPYRIGHT "1990-${DEV_CURRENT_YEAR} PCRaster Research and Development Team")
set(${PROJECT_NAME}_PROJECT_NAME "${PROJECT_NAME}")
set(${PROJECT_NAME}_HOMEPAGE "http://www.pcraster.eu")
set(${PROJECT_NAME}_MAJOR_VERSION "4")
set(${PROJECT_NAME}_MINOR_VERSION "2")
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


# configure_file(
#     ${CMAKE_CURRENT_SOURCE_DIR}/CTestCustom.cmake.in
#     ${CMAKE_CURRENT_BINARY_DIR}/CTestCustom.cmake
# )

include(GenerateExportHeader)


# this one should be removed after compiler warnings are fixed
include(PCRasterCompilerConfiguration)

if(PCRASTER_BUILD_TEST)
    # These executables need to be set, empty variables will cause pain at testing
    if(NOT GDAL_TRANSLATE)
        message(FATAL_ERROR "Could not find application " GDAL_TRANSLATE)
    endif()
    if(NOT SQLITE3_EXECUTABLE)
        message(FATAL_ERROR "Could not find application " SQLITE3_EXECUTABLE)
    endif()
endif()

add_subdirectory(source)
add_subdirectory(documentation)

# Create version information file
# Dummy target that does nothing. This enables us to attach build commands.
add_custom_target(version_info_dummy_target ALL)

add_custom_command(
    TARGET
      version_info_dummy_target
    PRE_BUILD
    COMMAND
      "${CMAKE_COMMAND}" -DVER=${pcraster_VERSION} -DSRC_DIR=${CMAKE_CURRENT_SOURCE_DIR} -P "${CMAKE_CURRENT_SOURCE_DIR}/environment/cmake/PCRasterVersionInformation.cmake"
)


install(
  FILES
    ${CMAKE_CURRENT_BINARY_DIR}/pcraster_version.txt
  DESTINATION
    doc
)
