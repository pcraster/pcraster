if(PCRASTER_BUILD_TEST)
    cmake_minimum_required(VERSION 3.7)
else()
    cmake_minimum_required(VERSION 3.3)
endif()

project(pcraster)

set(DEV_CURRENT_YEAR "2018")

set(${PROJECT_NAME}_ORGANISATION_NAME "PCRaster Research and Development Team")
set(${PROJECT_NAME}_COPYRIGHT
    "1990-${DEV_CURRENT_YEAR}, The ${${PROJECT_NAME}_ORGANISATION_NAME}")
set(${PROJECT_NAME}_PROJECT_NAME "${PROJECT_NAME}")
set(${PROJECT_NAME}_HOMEPAGE "http://www.pcraster.eu")
set(${PROJECT_NAME}_MAJOR_VERSION "4")
set(${PROJECT_NAME}_MINOR_VERSION "3")
set(${PROJECT_NAME}_PATCH_VERSION "0")
set(${PROJECT_NAME}_PRE_RELEASE_VERSION "-dev")
set(${PROJECT_NAME}_VERSION
    "${${PROJECT_NAME}_MAJOR_VERSION}.${${PROJECT_NAME}_MINOR_VERSION}.${${PROJECT_NAME}_PATCH_VERSION}${${PROJECT_NAME}_PRE_RELEASE_VERSION}")

set(CMAKE_MODULE_PATH
    $ENV{CMAKE_MODULE_PATH}
    ${CMAKE_CURRENT_SOURCE_DIR}/devbase/cmake
    ${CMAKE_CURRENT_SOURCE_DIR}/environment/cmake
)
include(PCRaster)

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
